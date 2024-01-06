use std::cell::UnsafeCell;

use super::value::Value;

pub struct MutableVariable(pub UnsafeCell<Value>);

impl std::fmt::Debug for MutableVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MutableVariable({:?})", unsafe { self.get() })
    }
}

impl MutableVariable {
    pub fn new(value: Value) -> Self {
        Self(UnsafeCell::new(value))
    }

    pub fn as_ref(&self) -> &Value {
        unsafe { &*self.0.get() }
    }

    pub unsafe fn get(&self) -> Value {
        self.0.get().read()
    }

    pub unsafe fn set(&self, value: Value) {
        *self.0.get() = value;
    }
}

#[derive(Debug, Clone)]
pub struct ImmutableVariable(pub Value);

impl ImmutableVariable {
    pub fn new(value: Value) -> Self {
        Self(value)
    }

    pub fn as_ref(&self) -> &Value {
        &self.0
    }

    pub fn set(&mut self, value: Value) {
        self.0 = value;
    }
}

#[derive(Debug)]
pub enum Variable {
    Mutable(MutableVariable),
    Immutable(ImmutableVariable),
}

const NONE: Value = Value::Pointer(usize::MAX);
impl Default for Variable {
    fn default() -> Self {
        Self::Immutable(ImmutableVariable(NONE))
    }
}

impl Variable {
    pub fn inside_ref(&self) -> &Value {
        match self {
            Self::Mutable(value) => value.as_ref().inside_ref(),
            Self::Immutable(value) => value.as_ref(),
        }
    }

    pub fn inside_cloned(&self) -> Value {
        match self {
            Self::Mutable(value) => value.as_ref().clone(),
            Self::Immutable(value) => value.as_ref().clone(),
        }
    }
}
