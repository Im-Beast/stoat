use crate::instruction::Instruction;

#[derive(Debug)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn new(instructions: Vec<Instruction>) -> Self {
        Self { instructions }
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn get(&self, index: usize) -> &Instruction {
        &self.instructions[index]
    }
}
