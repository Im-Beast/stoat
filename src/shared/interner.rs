use std::collections::HashMap;

pub type StringID = usize;

#[derive(Debug, Clone)]
pub struct Interner<'a> {
    map: HashMap<&'a str, StringID>,
    strings: Vec<String>,
}

impl Default for Interner<'_> {
    fn default() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }
}

impl<'a> Interner<'a> {
    pub fn intern(&mut self, name: &'a str) -> StringID {
        if let Some(id) = self.map.get(name) {
            return *id;
        }

        let id = self.strings.len();
        self.strings.push(name.to_string());
        self.map.insert(name, id);
        id
    }

    pub fn get(&self, id: StringID) -> Option<&String> {
        self.strings.get(id)
    }
}
