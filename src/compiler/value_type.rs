use lexer::Token;
use vm::value::Value;

#[derive(Debug)]
#[repr(u8)]
pub enum Type {
    UnknownInt(Token),
    I8,
    I16,
    I32,
    I64,

    U8,
    U16,
    U32,
    U64,

    UnknownFloat(Token),
    F32,
    F64,

    Reference(Box<Type>),
    MutableReference(Box<Type>),

    Vec(Box<Type>),
    Array(Box<Type>, u32),
    Tuple(Box<[Type]>),

    Bool,
    String,
    Char,
    Pointer,
}

impl From<&Value> for Type {
    fn from(value: &Value) -> Self {
        match value {
            Value::I8(_) => Self::I8,
            Value::I16(_) => Self::I16,
            Value::I32(_) => Self::I32,
            Value::I64(_) => Self::I64,

            Value::U8(_) => Self::U8,
            Value::U16(_) => Self::U16,
            Value::U32(_) => Self::U32,
            Value::U64(_) => Self::U64,

            Value::F32(_) => Self::F32,
            Value::F64(_) => Self::F64,

            Value::Bool(_) => Self::Bool,
            Value::String(_) => Self::String,
            Value::Char(_) => Self::Char,

            Value::Pointer(_) => Self::Pointer,
        }
    }
}

impl Type {
    pub fn is_typeof(&self, value: Value) -> bool {
        match (self, value) {
            (
                Self::UnknownInt(..),
                Value::I8(_)
                | Value::I16(_)
                | Value::I32(_)
                | Value::I64(_)
                | Value::U8(_)
                | Value::U16(_)
                | Value::U32(_)
                | Value::U64(_),
            ) => true,

            (Self::UnknownFloat(..), Value::F32(_) | Value::F64(_)) => true,

            (Self::I8, Value::I8(_)) => true,
            (Self::I16, Value::I16(_)) => true,
            (Self::I32, Value::I32(_)) => true,
            (Self::I64, Value::I64(_)) => true,

            (Self::U8, Value::U8(_)) => true,
            (Self::U16, Value::U16(_)) => true,
            (Self::U32, Value::U32(_)) => true,
            (Self::U64, Value::U64(_)) => true,

            (Self::F32, Value::F32(_)) => true,
            (Self::F64, Value::F64(_)) => true,

            (Self::Bool, Value::Bool(_)) => true,
            (Self::String, Value::String(_)) => true,

            (Self::Pointer, Value::Pointer(_)) => true,

            _ => false,
        }
    }
}
