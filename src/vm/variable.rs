use std::{cell::RefCell, rc::Rc};

use super::value::Value;

pub type MutableVariable = Rc<RefCell<Value>>;
pub type ImmutableVariable = Rc<Value>;

#[derive(Debug, Clone)]
pub enum Variable {
    Mutable(MutableVariable),
    Immutable(ImmutableVariable),
}

const NONE: Value = Value::Pointer(usize::MAX);
impl Default for Variable {
    fn default() -> Self {
        Self::Immutable(Rc::new(NONE))
    }
}

impl Variable {
    pub fn inside_value(&self) -> &Value {
        match self {
            Self::Mutable(v) => unsafe { &*v.as_ptr() },
            Self::Immutable(v) => &v,
        }
    }
}
