use crate::shared::interner::InternedString;

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub return_pointer: usize,
    pub variables: Vec<InternedString>, // InternedString -> InstructionPointer
}

impl CallFrame {
    pub fn new(return_pointer: usize) -> Self {
        Self {
            return_pointer,
            variables: Vec::new(),
        }
    }

    pub fn set_variable_pointer(&mut self, name: InternedString, ip: usize) {
        if self.variables.len() <= name {
            self.variables.resize(name + 1, usize::MAX);
        }

        self.variables[name] = ip;
    }

    pub fn get_variable_pointer(&self, name: InternedString) -> Option<&usize> {
        self.variables.get(name)
    }
}
