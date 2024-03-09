use super::value::Value;

#[derive(Debug, Clone)]
pub enum Instruction {
    PushStack(Value),
    PopStack,

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
