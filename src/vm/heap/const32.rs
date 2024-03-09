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
