use std::{cell::RefCell, rc::Rc};

use crate::{parser::FunctionArg, shared::types::Type};

use super::BuiltinFunctionClosure;

pub struct BuiltinFunctionValue<'a, 'b>(
    pub Box<[FunctionArg<'a>]>,                      // parameters (name, type)
    pub bool,                                        // variable number of arguments
    pub Type,                                        // return_type
    pub Rc<RefCell<BuiltinFunctionClosure<'a, 'b>>>, // body
);

impl<'a, 'b> std::fmt::Debug for BuiltinFunctionValue<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunctionValue")
            .field("parameters", &self.0)
            .field("variable_args", &self.1)
            .field("return_type", &self.2)
            .finish()
    }
}

impl<'a, 'b> Clone for BuiltinFunctionValue<'a, 'b> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1, self.2.clone(), self.3.clone())
    }
}

impl<'a, 'b> PartialEq for BuiltinFunctionValue<'a, 'b> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
