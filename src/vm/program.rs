use crate::vm::Instruction;
use crate::vm::Interner;

pub struct Program {
    pub instructions: Vec<Instruction>,
    pub interner: Interner,
}

impl Program {
    pub fn new(interner: Interner, instructions: Vec<Instruction>) -> Self {
        Self {
            interner,
            instructions,
        }
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn get(&self, index: usize) -> &Instruction {
        &self.instructions[index]
    }
}
