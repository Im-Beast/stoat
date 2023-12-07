use std::{cell::RefCell, rc::Rc};

use crate::shared::{name_bihasher::NameHash, no_hash_map::NoHashMap};

use super::{builtins::get_builtins, values::Value};

#[derive(Debug)]
pub enum LoopAction {
    Continue,
    Break,
}

#[derive(Debug)]
pub enum ScopeKind<'a> {
    Function(Option<Value<'a>>), // returned value
    Loop(Option<LoopAction>),    // whether loop should continue or break or keep running
    Block,
}

#[derive(Debug)]
pub struct Scope<'a> {
    pub kind: ScopeKind<'a>,
    pub variables: NoHashMap<Variable<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new(kind: ScopeKind<'a>) -> Scope<'a> {
        Scope {
            kind,
            variables: NoHashMap::default(),
        }
    }

    pub fn with_builtins(kind: ScopeKind<'a>) -> Scope<'a> {
        let mut scope = Scope::new(kind);
        scope.attach_builtins();
        scope
    }

    pub fn set_immutable_variable(&mut self, name_hash: NameHash, value: Value<'a>) {
        self.set_variable(name_hash, Variable::new(false, value));
    }

    #[allow(dead_code)]
    pub fn set_mutable_variable(&mut self, name_hash: NameHash, value: Value<'a>) {
        self.set_variable(name_hash, Variable::new(true, value));
    }

    pub fn set_variable(&mut self, name_hash: NameHash, variable: Variable<'a>) {
        self.variables.insert(name_hash, variable);
    }

    pub fn attach_builtins(&mut self) {
        for (name, value) in get_builtins() {
            self.set_immutable_variable(name, value);
        }
    }
}

#[derive(Debug)]
pub enum ValueReference<'a> {
    Mutable(Rc<RefCell<Value<'a>>>),
    Immutable(Rc<Value<'a>>),
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub mutable: bool,
    pub value: ValueReference<'a>,
}

impl<'a> Variable<'a> {
    pub fn new(mutable: bool, value: Value<'a>) -> Self {
        Self {
            mutable,
            value: if mutable {
                ValueReference::Mutable(Rc::new(RefCell::new(value)))
            } else {
                ValueReference::Immutable(Rc::new(value))
            },
        }
    }

    pub fn as_value_reference(&self) -> Value<'a> {
        match &self.value {
            ValueReference::Mutable(value) => Value::MutableReference(value.clone()),
            ValueReference::Immutable(value) => Value::Reference(value.clone()),
        }
    }
}
