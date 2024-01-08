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

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> &Instruction {
        &self.instructions[index]
    }

    #[inline(always)]
    pub fn get_mut(&mut self, index: usize) -> &mut Instruction {
        &mut self.instructions[index]
    }
}
