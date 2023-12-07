use std::{
    collections::HashMap,
    hash::{BuildHasher, Hasher},
};

#[derive(Debug, Clone, Copy)]
pub struct NoHasher {
    value: u64,
}

impl Hasher for NoHasher {
    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.value = i;
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.value
    }

    fn write(&mut self, _: &[u8]) {
        panic!("NoHasher only supports u64 as key type");
    }
}

impl Default for NoHasher {
    fn default() -> Self {
        Self { value: 0 }
    }
}

impl BuildHasher for NoHasher {
    type Hasher = Self;
    fn build_hasher(&self) -> Self::Hasher {
        *self
    }
}

pub type NoHashMap<V> = HashMap<u64, V, NoHasher>;
