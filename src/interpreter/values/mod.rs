mod array;
mod builtin_function;
mod exclusive_range;
mod inclusive_range;
mod slice;
pub use self::{array::*, builtin_function::*, exclusive_range::*, inclusive_range::*, slice::*};

use crate::{
    parser::{FunctionArg, StatementBody},
    shared::types::Type,
};

use miette::{bail, Result};
use std::{borrow::Cow, cell::RefCell, fmt::Display, rc::Rc};

#[macro_export]
macro_rules! primitives_pattern {
    () => {
        Value::Integer32(..)
            | Value::Integer64(..)
            | Value::Float32(..)
            | Value::Float64(..)
            | Value::Boolean(..)
            | Value::String(..)
            | Value::None
    };
}

#[macro_export]
macro_rules! extract_type_value {
    ($value:ident, $type:ident) => {
        let temp_from = $value.as_direct_ref();
        let Value::$type($value) = temp_from else {
            bail!("Expected type {:?} but got {:?}", stringify!($type), $value);
        };
    };

    ($from:expr, $into:ident, $type:ident) => {
        let temp_from = $from.as_direct_ref();
        let Value::$type($into) = temp_from else {
            bail!("Expected type {:?} but got {:?}", stringify!($type), $from);
        };
    };

    ($from:expr, mut $into:ident, $type:ident) => {
        let temp_from = $from.as_direct_mut();
        let Value::$type($into) = temp_from else {
            bail!("Expected type {:?} but got {:?}", stringify!($type), $from);
        };
    };
}

pub type BuiltinFunctionClosure<'a, 'b> =
    dyn FnMut(Box<[Cow<'b, Value<'a>>]>) -> Result<Value<'a>> + 'a;
pub type BuiltinIterator<'a, 'b> = dyn Iterator<Item = Cow<'b, Value<'a>>> + 'b;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionValue<'a>(
    pub Box<[FunctionArg<'a>]>, // parameters (name, type)
    pub Type,                   // return_type
    pub StatementBody<'a>,      // body
);

#[derive(Debug, Clone, PartialEq)]
pub struct TupleValue<'a, 'b>(pub Box<[Cow<'b, Value<'a>>]>);

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    MutableReference(Rc<RefCell<Value<'a>>>),
    Reference(Rc<Value<'a>>),

    // numbers
    Integer32(i32),
    Integer64(i64),
    Float32(f32),
    Float64(f64),
    // ranges
    ExclusiveRange(ExclusiveRangeValue<'a>), // from, to
    InclusiveRange(InclusiveRangeValue<'a>), // from, to
    // bool
    Boolean(bool),
    // fun
    Function(FunctionValue<'a>), // parameters (name, type), return_type, body
    BuiltinFunction(BuiltinFunctionValue<'a, 'a>), // parameters (name, type), return_type, body
    // string
    String(String),
    // Built-in objects
    Array(ArrayValue<'a, 'a>),
    Slice(SliceValue<'a, 'a>),
    Tuple(TupleValue<'a, 'a>),
    // TODO: Structs, enums and stuff
    Custom(bool),
    None,
}

impl<'a> Value<'a> {
    pub fn as_direct_ref(&self) -> &Value<'a> {
        match self {
            Value::Reference(value) => value,
            Value::MutableReference(value) => unsafe { &*value.as_ref().as_ptr() },
            value => value,
        }
    }

    pub fn as_direct_mut(&mut self) -> &mut Value<'a> {
        match self {
            Value::Reference(_) => unreachable!(),
            Value::MutableReference(value) => unsafe { &mut *value.as_ptr() },
            value => value,
        }
    }

    // check if type equals
    pub fn is_type_of(&self, value_type: &Type) -> bool {
        match self {
            Value::Integer32(_) => *value_type == Type::Integer32,
            Value::Integer64(_) => *value_type == Type::Integer64,
            Value::Float32(_) => *value_type == Type::Float32,
            Value::Float64(_) => *value_type == Type::Float64,

            Value::Boolean(_) => *value_type == Type::Boolean,
            Value::String(_) => *value_type == Type::String,

            Value::InclusiveRange(..) => *value_type == Type::InclusiveRange,
            Value::ExclusiveRange(..) => *value_type == Type::ExclusiveRange,

            Value::Reference(value) => value.as_ref().is_type_of(value_type),
            Value::MutableReference(value) => value.borrow().is_type_of(value_type),
            Value::None => *value_type == Type::None,

            Value::Array(ArrayValue(items, size)) => {
                if let Type::Array(value_type, type_size) = value_type {
                    if *size != *type_size {
                        return false;
                    }
                    items.iter().all(|value| value.is_type_of(value_type))
                } else {
                    false
                }
            }
            Value::Slice(SliceValue(values)) => {
                if let Type::Slice(value_type) = value_type {
                    values.iter().all(|value| value.is_type_of(value_type))
                } else {
                    false
                }
            }
            Value::Tuple(TupleValue(values)) => {
                if let Type::Tuple(value_types) = value_type {
                    values
                        .iter()
                        .enumerate()
                        .all(|(i, value)| value.is_type_of(&value_types[i]))
                } else {
                    false
                }
            }

            value => todo!("is_type_of {value:?}"),
        }
    }

    pub fn assert_type_of(&self, value_type: &Type) -> Result<()> {
        if self.is_type_of(value_type) {
            Ok(())
        } else {
            bail!("Expected type {:?} but got {:?}", value_type, self);
        }
    }
}

impl<'a> From<&Value<'a>> for Type {
    fn from(value: &Value<'a>) -> Self {
        match value {
            Value::Integer32(_) => Type::Integer32,
            Value::Integer64(_) => Type::Integer64,
            Value::Float32(_) => Type::Float32,
            Value::Float64(_) => Type::Float64,
            Value::Boolean(_) => Type::Boolean,
            Value::String(_) => Type::String,
            Value::None => Type::None,

            Value::InclusiveRange(..) => Type::InclusiveRange,
            Value::ExclusiveRange(..) => Type::ExclusiveRange,

            Value::Reference(value) => value.as_ref().into(),
            Value::MutableReference(value) => {
                let value = value.borrow();
                (&*value).into()
            }

            Value::Array(ArrayValue(values, size)) => {
                let item_type: Type = values[0].as_ref().into();
                Type::Array(item_type.into(), *size)
            }
            Value::Slice(SliceValue(values)) => {
                let item_type: Type = values[0].as_ref().into();
                Type::Slice(item_type.into())
            }
            Value::Tuple(TupleValue(values)) => {
                Type::Tuple(values.iter().map(|value| value.as_ref().into()).collect())
            }

            Value::BuiltinFunction(BuiltinFunctionValue(parameters, _, return_type, _))
            | Value::Function(FunctionValue(parameters, return_type, _)) => {
                let parameters: Vec<Type> = parameters
                    .iter()
                    .map(|parameter| parameter.1.clone())
                    .collect();
                Type::Function(parameters.into(), (return_type.clone()).into())
            }
            Value::Custom(_) => todo!(),
        }
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer32(value) => write!(f, "{}", value),
            Value::Integer64(value) => write!(f, "{}", value),
            Value::Float32(value) => write!(f, "{}", value),
            Value::Float64(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::None => write!(f, "None"),
            Value::InclusiveRange(range) => write!(f, "{}..={}", range.0, range.1),
            Value::ExclusiveRange(range) => write!(f, "{}..{}", range.0, range.1),
            Value::Reference(value) => write!(f, "&{}", value),
            Value::MutableReference(value) => write!(f, "&mut {}", value.borrow()),
            Value::Array(ArrayValue(values, size)) => {
                let joined = values
                    .iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "[{joined}]{size}")
            }
            Value::Slice(SliceValue(values)) => {
                let joined = values
                    .iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "[|{joined}|]")
            }
            Value::Tuple(TupleValue(values)) => {
                let joined = values
                    .iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "({joined})")
            }
            Value::BuiltinFunction(_) => write!(f, "<builtin function>"),
            Value::Function(_) => write!(f, "<function>"),
            Value::Custom(_) => write!(f, "<custom>"),
        }
    }
}
