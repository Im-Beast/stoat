use super::value::Value;

#[derive(Debug)]
#[repr(u8)]
pub enum Instruction {
    Push(Value),
    Pop,

    Ref,
    Clone,

    DeclareLet,
    DeclareLetMut,

    Assign,

    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    JumpAbsolute,
    JumpName,

    Print,
}
