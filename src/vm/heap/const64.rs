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
