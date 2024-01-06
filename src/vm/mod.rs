use std::{cell::RefCell, rc::Rc};

use crate::shared::interner::{Interner, StringID};
use miette::{bail, Result};

mod stack;
use stack::Stack;

mod instruction;
use instruction::Instruction;

mod value;
use value::Value;

mod variable;
use variable::Variable;

pub fn vm_test() {
    let mut vm = VM::default();

    let dog = vm.iterner.intern("dog");
    let cat = vm.iterner.intern("cat");

    vm.program = vec![
        Instruction::Push(Value::I32(2)),
        Instruction::Push(Value::Pointer(dog)),
        Instruction::DeclareLetMut,
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Push(Value::I32(3)),
        Instruction::Add,
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Assign,
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Print,
        Instruction::Push(Value::Pointer(3)),
        Instruction::JumpAbsolute,
    ];

    /*     vm.program = vec![
        // let mut dog = 2;
        Instruction::Push(Value::I32(2)),
        Instruction::Push(Value::Pointer(dog)),
        Instruction::DeclareLetMut,
        // print(dog)
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Print,
        // print(dog + 3)
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Push(Value::I32(3)),
        Instruction::Add,
        Instruction::Print,
        // let cat = &dog;
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Push(Value::Pointer(cat)),
        Instruction::DeclareLet,
        // print(cat)
        Instruction::Push(Value::Pointer(cat)),
        Instruction::Ref,
        Instruction::Print,
        // print(cat + cat)
        Instruction::Push(Value::Pointer(cat)),
        Instruction::Ref,
        Instruction::Push(Value::Pointer(cat)),
        Instruction::Ref,
        Instruction::Add,
        Instruction::Print,
        // dog = dog + 3;
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Push(Value::I32(3)),
        Instruction::Add,
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Assign,
        // print(dog)
        Instruction::Push(Value::Pointer(dog)),
        Instruction::Ref,
        Instruction::Print,
        // print(cat)
        Instruction::Push(Value::Pointer(cat)),
        Instruction::Ref,
        Instruction::Print,
    ]; */

    vm.run().unwrap()
}

struct VM<'stack> {
    iterner: Interner,

    variables: Vec<Variable>, // <StringId, Variable>
    labels: Vec<usize>,       // <StringId, InstructionIndex>

    ip: usize,
    program: Vec<Instruction>,
    stack: Stack<'stack, Value>,
}

impl Default for VM<'_> {
    fn default() -> Self {
        Self {
            iterner: Interner::default(),
            variables: Vec::new(),
            labels: Vec::new(),
            ip: 0,
            program: Vec::new(),
            stack: Stack::default(),
        }
    }
}

macro_rules! deref {
    ($value: expr) => {{
        match $value {
            Value::Reference(v) => v.inside(),
            Value::MutableReference(v) => v.inside(),
            v => v,
        }
    }};
    (ref; $value: expr) => {{
        match $value {
            Value::Reference(v) => v.inside_ref(),
            Value::MutableReference(v) => v.inside_ref(),
            v => v,
        }
    }};
    (cloned; $value: expr) => {{
        match $value {
            Value::Reference(v) => v.inside_cloned(),
            Value::MutableReference(v) => v.inside_cloned(),
            v => v,
        }
    }};
}

macro_rules! binary_operation {
    ($self: expr, $operand: tt) => {{
        let a = deref!(cloned; $self.stack.pop());
        let b = deref!(cloned; $self.stack.pop());

        match (a, b) {
            (Value::I8(a), Value::I8(b)) => $self.stack.push(Value::I8(b $operand a)),
            (Value::I16(a), Value::I16(b)) => $self.stack.push(Value::I16(b $operand a)),
            (Value::I32(a), Value::I32(b)) => $self.stack.push(Value::I32(b $operand a)),
            (Value::I64(a), Value::I64(b)) => $self.stack.push(Value::I64(b $operand a)),

            (Value::U8(a), Value::U8(b)) => $self.stack.push(Value::U8(b $operand a)),
            (Value::U16(a), Value::U16(b)) => $self.stack.push(Value::U16(b $operand a)),
            (Value::U32(a), Value::U32(b)) => $self.stack.push(Value::U32(b $operand a)),
            (Value::U64(a), Value::U64(b)) => $self.stack.push(Value::U64(b $operand a)),

            (Value::F32(a), Value::F32(b)) => $self.stack.push(Value::F32(b $operand a)),
            (Value::F64(a), Value::F64(b)) => $self.stack.push(Value::F64(b $operand a)),

            (a, b) => panic!("Cannot perform binary operation on non-numeric and mixed values. Got {a:?} and {b:?}"),
        }
    }}
}

impl<'stack> VM<'stack> {
    pub fn run(&mut self) -> Result<()> {
        if self.ip >= self.program.len() {
            bail!("Attempted to run an empty program.")
        }

        while self.ip < self.program.len() {
            let instruction = &self.program[self.ip];
            self.ip += 1;

            match instruction {
                Instruction::Push(value) => self.stack.push(value.to_owned()),
                Instruction::Pop => {
                    self.stack.pop();
                }

                Instruction::Ref => {
                    let pointer = self.stack.pop();
                    let variable = &self.variables[pointer.as_usize()];
                    self.stack.push(Value::Reference(Rc::new(variable.clone())))
                }
                Instruction::Clone => {
                    let pointer = self.stack.pop();
                    let variable = &self.variables[pointer.as_usize()];
                    self.stack.push(variable.inside_ref().to_owned());
                }

                Instruction::DeclareLet => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();

                    self.push_variable(name.as_usize(), Variable::Immutable(Rc::new(value)));
                }
                Instruction::DeclareLetMut => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();

                    self.push_variable(
                        name.as_usize(),
                        Variable::Mutable(Rc::new(RefCell::new(value))),
                    );
                }

                Instruction::Assign => {
                    let pointer = self.stack.pop();
                    let value = self.stack.pop();

                    let variable = self.get_variable(pointer.as_usize());
                    let Variable::Mutable(variable) = variable else {
                        panic!("Attempted to assign to an immutable variable.")
                    };

                    *variable.borrow_mut() = value;
                }

                Instruction::Add => binary_operation!(self, +),
                Instruction::Subtract => binary_operation!(self, -),
                Instruction::Multiply => binary_operation!(self, *),
                Instruction::Divide => binary_operation!(self, /),
                Instruction::Modulo => binary_operation!(self, %),

                Instruction::JumpAbsolute => {
                    let ip = self.stack.pop();
                    self.jump_abs(ip.into());
                }
                Instruction::JumpName => {
                    let name = self.stack.pop();
                    self.jump_name(name.as_string());
                }

                Instruction::Print => {
                    let value = self.stack.pop();
                    println!("{:?}", value);
                }
            }
        }

        Ok(())
    }

    pub fn push_variable(&mut self, interned: StringID, variable: Variable) -> StringID {
        // Make sure we have enough space
        self.variables.resize(interned + 1, Variable::default());
        self.variables[interned] = variable;
        interned
    }

    pub fn get_variable(&self, interned: StringID) -> &Variable {
        self.variables
            .get(interned)
            .expect("Attempted to get a variable that does not exist.")
    }

    pub fn label(&mut self, name: &str) -> StringID {
        let interned = self.iterner.intern(name);
        // Make sure we have enough space
        self.labels.resize(interned, 0);
        self.labels[interned] = self.ip;
        interned
    }

    pub fn jump_abs(&mut self, ip: usize) {
        self.ip = ip;
    }

    pub fn jump_name(&mut self, name: &str) {
        let interned = self.iterner.intern(name);
        self.ip = self.labels[interned];
    }
}
