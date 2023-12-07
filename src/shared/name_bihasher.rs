use std::hash::Hasher;

use rustc_hash::FxHasher;

use super::no_hash_map::NoHashMap;

pub type NameHash = u64;

pub struct NameBihasher<T: Hasher> {
    pub hasher: fn() -> T,
    hashed_names: NoHashMap<String>,
}

impl<T: Hasher> std::fmt::Debug for NameBihasher<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.hashed_names)?;
        Ok(())
    }
}

impl<T: Hasher> NameBihasher<T> {
    pub fn hash(&mut self, name: &str) -> NameHash {
        let mut hasher = (self.hasher)();
        hasher.write(name.as_bytes());
        let hash = hasher.finish();

        if !self.hashed_names.contains_key(&hash) {
            self.hashed_names.insert(hash, name.into());
        }

        hash
    }

    pub fn unhash(&self, hash: &NameHash) -> Option<&String> {
        self.hashed_names.get(hash)
    }
}

impl Default for NameBihasher<FxHasher> {
    fn default() -> Self {
        Self {
            hasher: || FxHasher::default(),
            hashed_names: NoHashMap::default(),
        }
    }
}

#[macro_export]
macro_rules! hash {
    ($type:ty,$data:expr) => {{
        use std::hash::Hasher;
        let mut hasher: $type = Default::default();
        hasher.write(&$data.as_bytes());
        hasher.finish()
    }};
}
