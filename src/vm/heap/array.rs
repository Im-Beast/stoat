use std::marker::PhantomData;

use super::{Heap, RelativePtr, RELATIVE_POINTER_NULL};
use crate::relative_ptr;

#[derive(Debug, Default)]
pub struct HeapArray<'a, const N: usize, T: Into<[u8; N]> + Copy> {
    pub ptr: RelativePtr,
    pub len: u32,
    phantom: PhantomData<&'a T>,
}

impl<const N: usize, T: Into<[u8; N]> + Copy> HeapArray<'_, N, T> {
    pub fn new(ptr: u32, len: u32) -> Self {
        Self {
            ptr,
            len,
            phantom: PhantomData,
        }
    }

    // Get the value at the given index in the array
    pub fn get(&self, heap: &Heap, index: u32) -> Option<&T> {
        if index >= self.len {
            return None;
        }
        unsafe {
            let ptr = relative_ptr!(bytes; *const T, heap.u8, self.ptr);
            // as_ref() already checks for null pointer
            ptr.add(index as usize).as_ref()
        }
    }

    // Set the value at the given index in the array
    pub fn set(&self, heap: &Heap, index: u32, value: T) -> Option<()> {
        if index >= self.len {
            return None;
        }

        unsafe {
            let ptr = relative_ptr!(bytes; *mut T, heap.u8, self.ptr);
            // as_mut() already checks for null pointer
            *ptr.add(index as usize).as_mut()? = value;
        }

        Some(())
    }

    // Get the reference to the array in the heap
    pub fn as_slice(&self, heap: &Heap) -> Option<&[T]> {
        let ptr = unsafe { relative_ptr!(bytes; *const T, heap.u8, self.ptr) };
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { std::slice::from_raw_parts(ptr, self.len as usize) })
        }
    }

    // Replace the current array with a new array
    pub fn set_array(&mut self, heap_bytes: &mut Vec<u8>, array: &[T]) -> Option<()> {
        let heap_arr_len = self.len as usize;
        let heap_arr_ptr = self.ptr as usize;

        let size_of = std::mem::size_of::<T>();
        let current_size = heap_arr_len * size_of;
        let new_size = array.len() * size_of;

        // If the current array finishes at the end of the heap
        // We don't need to reallocate the heap
        if heap_bytes.len() == current_size {
            if array.len() > heap_arr_len {
                // Extend the heap with the parts of new array that don't fit in the current array
                heap_bytes.extend(
                    array
                        .iter()
                        .skip(heap_arr_len)
                        .flat_map(|value| (*value).into()),
                );
            } else if array.len() < heap_arr_len {
                // Truncate the heap so it doesn't occupy more space than necessary
                heap_bytes.truncate(heap_arr_ptr + new_size);
            }

            // Replace chunk of the current array with new array
            heap_bytes.splice(
                heap_arr_ptr..heap_arr_ptr + current_size.min(new_size),
                array.iter().flat_map(|value| (*value).into()),
            );

            self.len = array.len() as u32;
            return Some(());
        }

        // If the new array is smaller or equal to the current array
        // We can just copy the new values and truncate the heap
        // Reallocation is not needed
        // We don't need to care about the old heap array
        // Because it will get garbage collected
        if array.len() <= heap_arr_len {
            // When new array is smaller than the current array
            // We can just reuse the current array's heap space
            heap_bytes.splice(
                heap_arr_ptr..heap_arr_ptr + new_size,
                array.iter().flat_map(|value| (*value).into()),
            );
            self.len = array.len() as u32;
            return Some(());
        }

        // If the new array is larger than the current array
        // And the current array does not finish at the end of the heap
        // We need to reallocate the heap
        // We don't need to care about the old heap array
        // Because it will get garbage collected
        self.ptr = heap_bytes.len() as u32;
        self.len = array.len() as u32;
        heap_bytes.extend(array.iter().flat_map(|value| (*value).into()));

        Some(())
    }
}
