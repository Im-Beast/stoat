use super::value::Value;

#[derive(Debug, Clone)]
pub enum Instruction {
    Push(Value),
    Pop,

    Ref,
    Clone,
    Duplicate,

    DeclareVariable,

    Assign,

    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Jump,
    JumpAbsolute,
    JumpIfEqual,

    Call,
    Return,

    Compare,

    Print,
}
