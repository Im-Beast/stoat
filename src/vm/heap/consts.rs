pub use crate::heap::const32::Const32;
pub use crate::heap::const64::Const64;

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
