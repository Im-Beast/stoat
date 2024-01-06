use std::rc::Rc;

use super::variable::{MutableVariable, Variable};

#[derive(Debug, Clone)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),

    F32(f32),
    F64(f64),

    Bool(bool),
    String(String),

    Pointer(usize),
    Reference(Rc<Variable>),
    MutableReference(Rc<MutableVariable>),
}

macro_rules! into_num_impl {
    ($conv_name: ident, $ty: ty) => {
        impl Value {
            pub fn $conv_name(&self) -> $ty {
                match self {
                    Value::I8(v) => *v as $ty,
                    Value::I16(v) => *v as $ty,
                    Value::I32(v) => *v as $ty,
                    Value::I64(v) => *v as $ty,
                    Value::U8(v) => *v as $ty,
                    Value::U16(v) => *v as $ty,
                    Value::U32(v) => *v as $ty,
                    Value::U64(v) => *v as $ty,
                    Value::F32(v) => *v as $ty,
                    Value::F64(v) => *v as $ty,
                    Value::Pointer(v) => *v as $ty,
                    Value::Reference(v) => v.inside_value().into(),
                    Value::Bool(_) => panic!("Cannot convert boolean to integer"),
                    Value::String(_) => panic!("Cannot convert string to integer i8"),
                    Value::MutableReference(_) => panic!("Cannot convert mutable reference to i8"),
                }
            }
        }

        impl Into<$ty> for &Value {
            fn into(self) -> $ty {
                self.$conv_name()
            }
        }

        impl Into<$ty> for Value {
            fn into(self) -> $ty {
                (&self).into()
            }
        }
    };
}

into_num_impl!(as_i8, i8);
into_num_impl!(as_i16, i16);
into_num_impl!(as_i32, i32);
into_num_impl!(as_i64, i64);
into_num_impl!(as_u8, u8);
into_num_impl!(as_u16, u16);
into_num_impl!(as_u32, u32);
into_num_impl!(as_u64, u64);
into_num_impl!(as_f32, f32);
into_num_impl!(as_f64, f64);
into_num_impl!(as_usize, usize);

impl Value {
    pub fn as_string(&self) -> &str {
        match self {
            Self::String(v) => v,
            _ => panic!("Cannot convert non-strings to string"),
        }
    }
}
