pub mod array;
pub mod consts;

use array::HeapArray;
use consts::{Const32, Const64};
use std::mem;

use self::consts::Const;

pub type RelativePtr = u32;
pub const RELATIVE_POINTER_NULL: RelativePtr = RelativePtr::MAX;

#[macro_export]
macro_rules! relative_ptr {
    (ptr; $type:ty, $ptr:expr, $offset:expr) => {
        if $offset == RELATIVE_POINTER_NULL {
            std::ptr::null_mut()
        } else {
            $ptr.add($offset as usize) as $type
        }
    };

    (bytes; $type: ty, $bytes:expr, $offset:expr) => {
        if $offset == RELATIVE_POINTER_NULL {
            std::ptr::null_mut()
        } else {
            $bytes.as_ptr().add($offset as usize) as $type
        }
    };
}

#[derive(Debug, Default)]
pub struct Heap<'a> {
    pub u8: Vec<u8>,

    pub c32: Vec<RelativePtr>,
    pub c64: Vec<RelativePtr>,

    pub c32arrays: Vec<HeapArray<'a, 4, Const32>>,
    pub c64arrays: Vec<HeapArray<'a, 8, Const64>>,

    pub tuples: Vec<HeapArray<'a, 8, Const>>,
}

impl Heap<'_> {
    pub fn gc(&mut self) {
        let c32_with_len = self
            .c32
            .iter_mut()
            .filter(|ptr| unsafe {
                !relative_ptr!(bytes; *const Const32, self.u8, **ptr).is_null()
            })
            .map(|ptr| (ptr, mem::size_of::<Const32>()));
        let c64_with_len = self
            .c64
            .iter_mut()
            .filter(|ptr| unsafe {
                !relative_ptr!(bytes; *const Const64, self.u8, **ptr).is_null()
            })
            .map(|ptr| (ptr, mem::size_of::<Const64>()));
        let c32arrays_with_len = self
            .c32arrays
            .iter_mut()
            .filter(|arr| unsafe {
                !relative_ptr!(bytes; *const Const32, self.u8, arr.ptr).is_null()
            })
            .map(|arr| (&mut arr.ptr, arr.len as usize * mem::size_of::<Const32>()));
        let c64arrays_with_len = self
            .c64arrays
            .iter_mut()
            .filter(|arr| unsafe {
                !relative_ptr!(bytes; *const Const64, self.u8, arr.ptr).is_null()
            })
            .map(|arr| (&mut arr.ptr, arr.len as usize * mem::size_of::<Const64>()));

        let mut new_heap = Vec::new();
        for (ptr, len) in c32_with_len
            .chain(c64_with_len)
            .chain(c32arrays_with_len)
            .chain(c64arrays_with_len)
        {
            let heap_ptr = *ptr as usize;
            let new_ptr = new_heap.len();
            new_heap.extend_from_slice(&self.u8[heap_ptr..heap_ptr + len]);
            *ptr = new_ptr as u32;
        }
        self.u8 = new_heap;
    }

    pub fn add_u8(&mut self, value: u8) -> usize {
        let index = self.u8.len();
        self.u8.push(value);
        index
    }

    pub fn get_u8(&self, index: usize) -> Option<&u8> {
        self.u8.get(index)
    }

    pub fn set_u8(&mut self, index: usize, value: u8) {
        self.u8[index] = value;
    }

    pub fn add_c32(&mut self, value: Const32) -> usize {
        // If an empty pointer field is present, we can reuse it
        let null_index = self
            .c32
            .iter()
            .position(|ptr| ptr == &RELATIVE_POINTER_NULL);
        self.u8.extend(value.0);

        if let Some(index) = null_index {
            self.c32[index] = (index * mem::size_of::<Const32>()) as u32;
            return index;
        }

        let index = self.c32.len();
        self.c32.push((index * mem::size_of::<Const32>()) as u32);
        index
    }

    pub fn get_c32(&self, index: usize) -> Option<Const32> {
        unsafe {
            let ptr = relative_ptr!(bytes; *const Const32, self.u8, *self.c32.get(index)?);
            // as_ref() already checks for null pointer
            Some(*ptr.as_ref()?)
        }
    }

    pub fn set_c32(&mut self, index: usize, value: Const32) -> Option<()> {
        unsafe {
            // as_mut() already checks for null pointer
            *relative_ptr!(bytes; *mut Const32, self.u8, self.c32[index]).as_mut()? = value;
        }
        Some(())
    }

    pub fn add_c64(&mut self, value: Const64) -> usize {
        // If an empty pointer field is present, we can reuse it
        let null_index = self
            .c64
            .iter()
            .position(|ptr| ptr == &RELATIVE_POINTER_NULL);
        self.u8.extend(value.0);

        if let Some(index) = null_index {
            self.c64[index] = (index * mem::size_of::<Const64>()) as u32;
            return index;
        }

        let index = self.c64.len();
        self.c64.push((index * mem::size_of::<Const64>()) as u32);
        index
    }

    pub fn get_c64(&self, index: usize) -> Option<Const64> {
        unsafe {
            let ptr = relative_ptr!(bytes; *const Const64, self.u8, *self.c64.get(index)?);
            // as_ref() already checks for null pointer
            Some(*ptr.as_ref()?)
        }
    }

    pub fn set_c64(&mut self, index: usize, value: Const64) {
        unsafe {
            *relative_ptr!(bytes; *mut Const64, self.u8, self.c64[index]) = value;
        }
    }

    pub fn add_c32array(&mut self, array: &[Const32]) -> usize {
        let null_index = self.c32arrays.iter().position(|arr| unsafe {
            relative_ptr!(bytes; *const Const32, self.u8, arr.ptr).is_null()
        });
        let ptr = self.u8.len();
        self.u8.extend(array.iter().flat_map(|value| value.0));

        if let Some(index) = null_index {
            self.c32arrays[index] = HeapArray::new(ptr as u32, array.len() as u32);
            return index;
        }

        let index = self.c32arrays.len();
        self.c32arrays
            .push(HeapArray::new(ptr as u32, array.len() as u32));
        index
    }

    pub fn get_c32array(&self, index: usize) -> Option<&HeapArray<4, Const32>> {
        self.c32arrays.get(index)
    }

    pub fn set_c32array(&mut self, index: usize, array: &[Const32]) -> Option<()> {
        let heap_array = self.c32arrays.get_mut(index)?;
        heap_array.set_array(&mut self.u8, array)
    }

    pub fn add_c64array(&mut self, array: &[Const64]) -> usize {
        let null_index = self.c64arrays.iter().position(|arr| unsafe {
            relative_ptr!(bytes; *const Const64, self.u8, arr.ptr).is_null()
        });
        let ptr = self.u8.len();
        self.u8.extend(array.iter().flat_map(|value| value.0));

        if let Some(index) = null_index {
            self.c64arrays[index] = HeapArray::new(ptr as u32, array.len() as u32);
            return index;
        }

        let index = self.c64arrays.len();
        self.c64arrays
            .push(HeapArray::new(ptr as u32, array.len() as u32));
        index
    }

    pub fn get_c64array(&self, index: usize) -> Option<&HeapArray<8, Const64>> {
        self.c64arrays.get(index)
    }

    pub fn set_c64array(&mut self, index: usize, array: &[Const64]) -> Option<()> {
        let heap_array = self.c64arrays.get_mut(index)?;
        heap_array.set_array(&mut self.u8, array)
    }
}

#[cfg(test)]
mod tests {
    use super::RELATIVE_POINTER_NULL;
    use crate::*;

    #[test]
    fn heap_c32() {
        let mut heap = Heap::default();
        let a = heap.add_c32(69u32.into());
        let b = heap.add_c32(420u32.into());
        let c = heap.add_c32(1337u32.into());
        {
            let a: u32 = heap.get_c32(a).unwrap().into();
            let b: u32 = heap.get_c32(b).unwrap().into();
            let c: u32 = heap.get_c32(c).unwrap().into();
            assert_eq!(a, 69);
            assert_eq!(b, 420);
            assert_eq!(c, 1337);
        }

        heap.set_c32(a, (2_147_483_647u32).into());
        heap.set_c32(b, (2_147_483_648u32).into());
        heap.set_c32(c, (10u32).into());
        {
            let a: u32 = heap.get_c32(a).unwrap().into();
            let b: i32 = heap.get_c32(b).unwrap().into();
            let c: i32 = heap.get_c32(c).unwrap().into();
            assert_eq!(a, 2_147_483_647);
            assert_eq!(b, -2_147_483_648);
            assert_eq!(c, 10);
        }

        heap.c32[b] = RELATIVE_POINTER_NULL;
        let before_gc = heap.u8.len();
        heap.gc();
        assert!(heap.u8.len() < before_gc);

        {
            let a: u32 = heap.get_c32(a).unwrap().into();
            let b = heap.get_c32(b);
            let c: i32 = heap.get_c32(c).unwrap().into();
            assert_eq!(a, 2_147_483_647);
            assert_eq!(b, None);
            assert_eq!(c, 10);
        }
    }

    #[test]
    fn heap_c64() {
        let mut heap = Heap::default();
        let a = heap.add_c64(69u64.into());
        let b = heap.add_c64(420u64.into());
        let c = heap.add_c64(1337u64.into());
        {
            let a: u64 = heap.get_c64(a).unwrap().into();
            let b: u64 = heap.get_c64(b).unwrap().into();
            let c: u64 = heap.get_c64(c).unwrap().into();
            assert_eq!(a, 69);
            assert_eq!(b, 420);
            assert_eq!(c, 1337);
        }

        heap.set_c64(a, (9_223_372_036_854_775_807u64).into());
        heap.set_c64(b, (9_223_372_036_854_775_808u64).into());
        heap.set_c64(c, (10u64).into());
        {
            let a: u64 = heap.get_c64(a).unwrap().into();
            let b: i64 = heap.get_c64(b).unwrap().into();
            let c: i64 = heap.get_c64(c).unwrap().into();
            assert_eq!(a, 9_223_372_036_854_775_807);
            assert_eq!(b, -9_223_372_036_854_775_808);
            assert_eq!(c, 10);
        }

        heap.c64[b] = RELATIVE_POINTER_NULL;
        let before_gc = heap.u8.len();
        heap.gc();
        assert!(heap.u8.len() < before_gc);

        {
            let a: u64 = heap.get_c64(a).unwrap().into();
            let b = heap.get_c64(b);
            let c: i64 = heap.get_c64(c).unwrap().into();
            assert_eq!(a, 9_223_372_036_854_775_807);
            assert_eq!(b, None);
            assert_eq!(c, 10);
        }
    }

    #[test]
    pub fn heap_c32_array() {
        let mut heap = Heap::default();
        let a = heap.add_c32array(&[1u32.into(), 2u32.into()]);
        let b = heap.add_c32array(&[
            3u32.into(),
            4u32.into(),
            5u32.into(),
            6u32.into(),
            7u32.into(),
        ]);
        let c = heap.add_c32array(&[
            8u32.into(),
            9u32.into(),
            10u32.into(),
            11u32.into(),
            12u32.into(),
            13u32.into(),
            14u32.into(),
            15u32.into(),
            16u32.into(),
            17u32.into(),
            18u32.into(),
            19u32.into(),
        ]);

        {
            let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
            let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
            let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

            assert_eq!(a, &[1u32.into(), 2u32.into()]);
            assert_eq!(
                b,
                &[
                    3u32.into(),
                    4u32.into(),
                    5u32.into(),
                    6u32.into(),
                    7u32.into()
                ]
            );
            assert_eq!(
                c,
                &[
                    8u32.into(),
                    9u32.into(),
                    10u32.into(),
                    11u32.into(),
                    12u32.into(),
                    13u32.into(),
                    14u32.into(),
                    15u32.into(),
                    16u32.into(),
                    17u32.into(),
                    18u32.into(),
                    19u32.into(),
                ]
            );
        }

        for _ in 0..2 {
            heap.set_c32array(a, &[3u32.into(), 4u32.into()]);
            heap.set_c32array(
                b,
                &[
                    5u32.into(),
                    6u32.into(),
                    7u32.into(),
                    8u32.into(),
                    9u32.into(),
                ],
            );
            heap.set_c32array(
                c,
                &[
                    10u32.into(),
                    11u32.into(),
                    12u32.into(),
                    13u32.into(),
                    14u32.into(),
                    15u32.into(),
                    16u32.into(),
                    17u32.into(),
                    18u32.into(),
                    19u32.into(),
                    20u32.into(),
                    21u32.into(),
                ],
            );

            {
                let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[3u32.into(), 4u32.into()]);
                assert_eq!(
                    b,
                    &[
                        5u32.into(),
                        6u32.into(),
                        7u32.into(),
                        8u32.into(),
                        9u32.into()
                    ]
                );
                assert_eq!(
                    c,
                    &[
                        10u32.into(),
                        11u32.into(),
                        12u32.into(),
                        13u32.into(),
                        14u32.into(),
                        15u32.into(),
                        16u32.into(),
                        17u32.into(),
                        18u32.into(),
                        19u32.into(),
                        20u32.into(),
                        21u32.into(),
                    ]
                );
            }

            heap.set_c32array(a, &[22u32.into(), 23u32.into()]);
            heap.set_c32array(b, &[24u32.into(), 25u32.into()]);
            heap.set_c32array(
                c,
                &[
                    26u32.into(),
                    27u32.into(),
                    28u32.into(),
                    29u32.into(),
                    30u32.into(),
                    31u32.into(),
                    32u32.into(),
                    33u32.into(),
                    34u32.into(),
                    35u32.into(),
                    36u32.into(),
                    37u32.into(),
                    38u32.into(),
                    39u32.into(),
                    40u32.into(),
                    41u32.into(),
                    42u32.into(),
                    43u32.into(),
                    44u32.into(),
                    45u32.into(),
                ],
            );

            {
                let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[22u32.into(), 23u32.into()]);
                assert_eq!(b, &[24u32.into(), 25u32.into()]);
                assert_eq!(
                    c,
                    &[
                        26u32.into(),
                        27u32.into(),
                        28u32.into(),
                        29u32.into(),
                        30u32.into(),
                        31u32.into(),
                        32u32.into(),
                        33u32.into(),
                        34u32.into(),
                        35u32.into(),
                        36u32.into(),
                        37u32.into(),
                        38u32.into(),
                        39u32.into(),
                        40u32.into(),
                        41u32.into(),
                        42u32.into(),
                        43u32.into(),
                        44u32.into(),
                        45u32.into(),
                    ]
                );
            }

            heap.set_c32array(a, &[46u32.into(), 47u32.into()]);
            heap.set_c32array(b, &[48u32.into(), 49u32.into()]);
            heap.set_c32array(c, &[]);

            {
                let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[46u32.into(), 47u32.into()]);
                assert_eq!(b, &[48u32.into(), 49u32.into()]);
                assert_eq!(c, &[]);
            }

            heap.set_c32array(a, &[]);
            heap.set_c32array(b, &[]);
            heap.set_c32array(c, &[]);

            {
                let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[]);
                assert_eq!(b, &[]);
                assert_eq!(c, &[]);
            }

            heap.set_c32array(a, &[50u32.into()]);
            heap.set_c32array(b, &[51u32.into()]);
            heap.set_c32array(c, &[52u32.into()]);

            {
                let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[50u32.into()]);
                assert_eq!(b, &[51u32.into()]);
                assert_eq!(c, &[52u32.into()]);
            }

            let before_gc = heap.u8.len();
            heap.gc();
            assert!(heap.u8.len() < before_gc);

            // Check that GC didn't change the arrays
            {
                assert!(heap.c32arrays.len() == 3, "GC removed arrays");

                let a = heap.get_c32array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c32array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c32array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[50u32.into()]);
                assert_eq!(b, &[51u32.into()]);
                assert_eq!(c, &[52u32.into()]);
            }

            heap.set_c32array(a, &[53u32.into(), 54u32.into(), 55u32.into()]);
            {
                let a = heap.get_c32array(a).unwrap();
                assert_eq!(a.len, 3);
                assert_eq!(
                    a.as_slice(&heap).unwrap(),
                    &[53u32.into(), 54u32.into(), 55u32.into()]
                );
                assert_eq!(a.get(&heap, 0), Some(&53u32.into()));
                assert_eq!(a.get(&heap, 1), Some(&54u32.into()));
                assert_eq!(a.get(&heap, 2), Some(&55u32.into()));
                assert_eq!(a.get(&heap, 3), None);

                a.set(&heap, 0, 56u32.into());

                assert_eq!(a.get(&heap, 0), Some(&56u32.into()));
            }
        }
    }

    #[test]
    pub fn heap_c64_array() {
        let mut heap = Heap::default();
        let a = heap.add_c64array(&[1u64.into(), 2u64.into()]);
        let b = heap.add_c64array(&[
            3u64.into(),
            4u64.into(),
            5u64.into(),
            6u64.into(),
            7u64.into(),
        ]);
        let c = heap.add_c64array(&[
            8u64.into(),
            9u64.into(),
            10u64.into(),
            11u64.into(),
            12u64.into(),
            13u64.into(),
            14u64.into(),
            15u64.into(),
            16u64.into(),
            17u64.into(),
            18u64.into(),
            19u64.into(),
        ]);

        {
            let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
            let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
            let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

            assert_eq!(a, &[1u64.into(), 2u64.into()]);
            assert_eq!(
                b,
                &[
                    3u64.into(),
                    4u64.into(),
                    5u64.into(),
                    6u64.into(),
                    7u64.into()
                ]
            );
            assert_eq!(
                c,
                &[
                    8u64.into(),
                    9u64.into(),
                    10u64.into(),
                    11u64.into(),
                    12u64.into(),
                    13u64.into(),
                    14u64.into(),
                    15u64.into(),
                    16u64.into(),
                    17u64.into(),
                    18u64.into(),
                    19u64.into(),
                ]
            );
        }

        for _ in 0..2 {
            heap.set_c64array(a, &[3u64.into(), 4u64.into()]);
            heap.set_c64array(
                b,
                &[
                    5u64.into(),
                    6u64.into(),
                    7u64.into(),
                    8u64.into(),
                    9u64.into(),
                ],
            );
            heap.set_c64array(
                c,
                &[
                    10u64.into(),
                    11u64.into(),
                    12u64.into(),
                    13u64.into(),
                    14u64.into(),
                    15u64.into(),
                    16u64.into(),
                    17u64.into(),
                    18u64.into(),
                    19u64.into(),
                    20u64.into(),
                    21u64.into(),
                ],
            );

            {
                let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[3u64.into(), 4u64.into()]);
                assert_eq!(
                    b,
                    &[
                        5u64.into(),
                        6u64.into(),
                        7u64.into(),
                        8u64.into(),
                        9u64.into()
                    ]
                );
                assert_eq!(
                    c,
                    &[
                        10u64.into(),
                        11u64.into(),
                        12u64.into(),
                        13u64.into(),
                        14u64.into(),
                        15u64.into(),
                        16u64.into(),
                        17u64.into(),
                        18u64.into(),
                        19u64.into(),
                        20u64.into(),
                        21u64.into(),
                    ]
                );
            }

            heap.set_c64array(a, &[22u64.into(), 23u64.into()]);
            heap.set_c64array(b, &[24u64.into(), 25u64.into()]);
            heap.set_c64array(
                c,
                &[
                    26u64.into(),
                    27u64.into(),
                    28u64.into(),
                    29u64.into(),
                    30u64.into(),
                    31u64.into(),
                    32u64.into(),
                    33u64.into(),
                    34u64.into(),
                    35u64.into(),
                    36u64.into(),
                    37u64.into(),
                    38u64.into(),
                    39u64.into(),
                    40u64.into(),
                    41u64.into(),
                    42u64.into(),
                    43u64.into(),
                    44u64.into(),
                    45u64.into(),
                ],
            );

            {
                let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[22u64.into(), 23u64.into()]);
                assert_eq!(b, &[24u64.into(), 25u64.into()]);
                assert_eq!(
                    c,
                    &[
                        26u64.into(),
                        27u64.into(),
                        28u64.into(),
                        29u64.into(),
                        30u64.into(),
                        31u64.into(),
                        32u64.into(),
                        33u64.into(),
                        34u64.into(),
                        35u64.into(),
                        36u64.into(),
                        37u64.into(),
                        38u64.into(),
                        39u64.into(),
                        40u64.into(),
                        41u64.into(),
                        42u64.into(),
                        43u64.into(),
                        44u64.into(),
                        45u64.into(),
                    ]
                );
            }

            heap.set_c64array(a, &[46u64.into(), 47u64.into()]);
            heap.set_c64array(b, &[48u64.into(), 49u64.into()]);
            heap.set_c64array(c, &[]);

            {
                let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[46u64.into(), 47u64.into()]);
                assert_eq!(b, &[48u64.into(), 49u64.into()]);
                assert_eq!(c, &[]);
            }

            heap.set_c64array(a, &[]);
            heap.set_c64array(b, &[]);
            heap.set_c64array(c, &[]);

            {
                let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[]);
                assert_eq!(b, &[]);
                assert_eq!(c, &[]);
            }

            heap.set_c64array(a, &[50u64.into()]);
            heap.set_c64array(b, &[51u64.into()]);
            heap.set_c64array(c, &[52u64.into()]);

            {
                let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[50u64.into()]);
                assert_eq!(b, &[51u64.into()]);
                assert_eq!(c, &[52u64.into()]);
            }

            let before_gc = heap.u8.len();
            heap.gc();
            assert!(heap.u8.len() < before_gc);

            // Check that GC didn't change the arrays
            {
                assert!(heap.c64arrays.len() == 3, "GC removed arrays");

                let a = heap.get_c64array(a).unwrap().as_slice(&heap).unwrap();
                let b = heap.get_c64array(b).unwrap().as_slice(&heap).unwrap();
                let c = heap.get_c64array(c).unwrap().as_slice(&heap).unwrap();

                assert_eq!(a, &[50u64.into()]);
                assert_eq!(b, &[51u64.into()]);
                assert_eq!(c, &[52u64.into()]);
            }

            heap.set_c64array(a, &[53u64.into(), 54u64.into(), 55u64.into()]);
            {
                let a = heap.get_c64array(a).unwrap();
                assert_eq!(a.len, 3);
                assert_eq!(
                    a.as_slice(&heap).unwrap(),
                    &[53u64.into(), 54u64.into(), 55u64.into()]
                );
                assert_eq!(a.get(&heap, 0), Some(&53u64.into()));
                assert_eq!(a.get(&heap, 1), Some(&54u64.into()));
                assert_eq!(a.get(&heap, 2), Some(&55u64.into()));
                assert_eq!(a.get(&heap, 3), None);

                a.set(&heap, 0, 56u64.into());

                assert_eq!(a.get(&heap, 0), Some(&56u64.into()));
            }
        }
    }
}
