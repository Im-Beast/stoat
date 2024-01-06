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
    pub fn inside_ref(&self) -> &Value {
        match self {
            Self::Mutable(v) => unsafe { &*v.as_ptr() },
            Self::Immutable(v) => &v,
        }
    }

    pub fn inside(self) -> Value {
        match self {
            Self::Mutable(value) => match Rc::try_unwrap(value) {
                Ok(value) => value.into_inner(),
                Err(value) => value.as_ref().borrow().clone(),
            },
            Self::Immutable(value) => match Rc::try_unwrap(value) {
                Ok(value) => value,
                Err(value) => value.as_ref().clone(),
            },
        }
    }

    pub fn inside_cloned(&self) -> Value {
        match self {
            Self::Mutable(value) => value.borrow().clone(),
            Self::Immutable(value) => value.as_ref().clone(),
        }
    }
}
