#[derive(Debug, Copy)]
#[repr(u8)]
pub enum Const {
    Const32(Const32),
    Const64(Const64),
}

impl Clone for Const {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<Const> for [u8; 8] {
    fn from(value: Const) -> [u8; 8] {
        match value {
            Const::Const32(value) => {
                let mut result = [0; 8];
                result[..4].copy_from_slice(&value.0);
                result
            }
            Const::Const64(value) => value.into(),
        }
    }
}

impl From<Const32> for Const {
    fn from(value: Const32) -> Self {
        Self::Const32(value)
    }
}

impl From<Const64> for Const {
    fn from(value: Const64) -> Self {
        Self::Const64(value)
    }
}

#[derive(Debug, PartialEq, Eq, Copy)]
#[repr(transparent)]
pub struct Const32(pub [u8; 4]);

impl Clone for Const32 {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<Const32> for [u8; 4] {
    fn from(value: Const32) -> [u8; 4] {
        value.0
    }
}

impl From<i32> for Const32 {
    fn from(value: i32) -> Self {
        Self(value.to_ne_bytes())
    }
}

impl From<Const32> for i32 {
    fn from(value: Const32) -> i32 {
        i32::from_ne_bytes(value.0)
    }
}

impl From<u32> for Const32 {
    fn from(value: u32) -> Self {
        Self(value.to_ne_bytes())
    }
}

impl From<Const32> for u32 {
    fn from(value: Const32) -> u32 {
        u32::from_ne_bytes(value.0)
    }
}

impl From<f32> for Const32 {
    fn from(value: f32) -> Self {
        Self(value.to_ne_bytes())
    }
}

impl From<Const32> for f32 {
    fn from(value: Const32) -> f32 {
        f32::from_ne_bytes(value.0)
    }
}

#[derive(Debug, PartialEq, Eq, Copy)]
#[repr(transparent)]
pub struct Const64(pub [u8; 8]);

impl Clone for Const64 {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<Const64> for [u8; 8] {
    fn from(value: Const64) -> [u8; 8] {
        value.0
    }
}

impl From<i64> for Const64 {
    fn from(value: i64) -> Self {
        Self(value.to_ne_bytes())
    }
}

impl From<Const64> for i64 {
    fn from(value: Const64) -> i64 {
        i64::from_ne_bytes(value.0)
    }
}

impl From<u64> for Const64 {
    fn from(value: u64) -> Self {
        Self(value.to_ne_bytes())
    }
}

impl From<Const64> for u64 {
    fn from(value: Const64) -> u64 {
        u64::from_ne_bytes(value.0)
    }
}

impl From<f64> for Const64 {
    fn from(value: f64) -> Self {
        Self(value.to_ne_bytes())
    }
}

impl From<Const64> for f64 {
    fn from(value: Const64) -> f64 {
        f64::from_ne_bytes(value.0)
    }
}
