use std::collections::HashMap;

pub type InternedString = usize;

#[derive(Debug, Clone)]
pub struct Interner {
    map: HashMap<String, usize>,
    strings: Vec<String>,
}

impl Default for Interner {
    fn default() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }
}

impl Interner {
    pub fn intern(&mut self, name: &str) -> InternedString {
        if let Some(id) = self.map.get(name) {
            return *id;
        }

        let id = self.strings.len();
        self.map.insert(name.to_string(), id);
        self.strings.push(name.to_string());
        id
    }

    pub fn intern_string(&mut self, name: String) -> InternedString {
        if let Some(id) = self.map.get(&*name) {
            return *id;
        }

        let id = self.strings.len();
        self.strings.push(name.to_owned());
        self.map.insert(name, id);
        id
    }

    pub fn get(&self, id: InternedString) -> Option<&String> {
        self.strings.get(id)
    }
}
