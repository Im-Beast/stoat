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
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I8(a), Self::I8(b)) => a == b,
            (Self::I16(a), Self::I16(b)) => a == b,
            (Self::I32(a), Self::I32(b)) => a == b,
            (Self::I64(a), Self::I64(b)) => a == b,

            (Self::U8(a), Self::U8(b)) => a == b,
            (Self::U16(a), Self::U16(b)) => a == b,
            (Self::U32(a), Self::U32(b)) => a == b,
            (Self::U64(a), Self::U64(b)) => a == b,

            (Self::F32(a), Self::F32(b)) => a == b,
            (Self::F64(a), Self::F64(b)) => a == b,

            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,

            (Self::Pointer(a), Self::Pointer(b)) => a == b,

            _ => false,
        }
    }
}

macro_rules! match_numeric_group {
    ($a: expr, $b: expr, $op: tt, $err: literal) => {
        match ($a, $b) {
            (Value::I8(a), Value::I8(b)) => a $op b,
            (Value::I16(a), Value::I16(b)) => a $op b,
            (Value::I32(a), Value::I32(b)) => a $op b,
            (Value::I64(a), Value::I64(b)) => a $op b,
            (Value::U8(a), Value::U8(b)) => a $op b,
            (Value::U16(a), Value::U16(b)) => a $op b,
            (Value::U32(a), Value::U32(b)) => a $op b,
            (Value::U64(a), Value::U64(b)) => a $op b,
            (Value::F32(a), Value::F32(b)) => a $op b,
            (Value::F64(a), Value::F64(b)) => a $op b,
            (Value::Pointer(a), Value::Pointer(b)) => a $op b,
            _ => panic!($err),
        }
    };
}

impl PartialOrd for Value {
    fn ge(&self, other: &Self) -> bool {
        match_numeric_group!(self, other, >=, "Cannot compare non-numeric values")
    }

    fn gt(&self, other: &Self) -> bool {
        match_numeric_group!(self, other, >, "Cannot compare non-numeric values")
    }

    fn le(&self, other: &Self) -> bool {
        match_numeric_group!(self, other, <=, "Cannot compare non-numeric values")
    }

    fn lt(&self, other: &Self) -> bool {
        match_numeric_group!(self, other, <, "Cannot compare non-numeric values")
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::I8(a), Value::I8(b)) => a.partial_cmp(b),
            (Value::I16(a), Value::I16(b)) => a.partial_cmp(b),
            (Value::I32(a), Value::I32(b)) => a.partial_cmp(b),
            (Value::I64(a), Value::I64(b)) => a.partial_cmp(b),
            (Value::U8(a), Value::U8(b)) => a.partial_cmp(b),
            (Value::U16(a), Value::U16(b)) => a.partial_cmp(b),
            (Value::U32(a), Value::U32(b)) => a.partial_cmp(b),
            (Value::U64(a), Value::U64(b)) => a.partial_cmp(b),
            (Value::F32(a), Value::F32(b)) => a.partial_cmp(b),
            (Value::F64(a), Value::F64(b)) => a.partial_cmp(b),
            (Value::Pointer(a), Value::Pointer(b)) => a.partial_cmp(b),
            _ => panic!("Cannot compare non-numeric values"),
        }
    }
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
                    Value::Bool(_) => panic!("Cannot convert boolean to integer"),
                    Value::String(_) => panic!("Cannot convert string to integer"),
                }
            }
        }

        impl Into<$ty> for &Value {
            fn into(self) -> $ty {
                self.$conv_name()
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
