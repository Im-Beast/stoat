use std::{
    cell::{RefCell, UnsafeCell},
    rc::Rc,
};

use crate::{
    shared::interner::{Interner, StringID},
    vm::variable::{ImmutableVariable, MutableVariable},
};
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

    let a = vm.interner.intern("a");
    let b = vm.interner.intern("b");
    let i = vm.interner.intern("i");
    let temp = vm.interner.intern("temp");

    let loop_label = vm.label_name("loop", 12);
    let end_label = vm.label_name("end", 42);

    let amount = 91;

    vm.program = Vec::from([
        // fibonacci sequence till 30th number
        // a = 0
        Instruction::Push(Value::I64(0)),
        Instruction::Push(Value::Pointer(a)),
        Instruction::DeclareLetMut,
        // b = 1
        Instruction::Push(Value::I64(1)),
        Instruction::Push(Value::Pointer(b)),
        Instruction::DeclareLetMut,
        // temp = 0
        Instruction::Push(Value::I64(0)),
        Instruction::Push(Value::Pointer(temp)),
        Instruction::DeclareLetMut,
        // i = 0
        Instruction::Push(Value::I64(0)),
        Instruction::Push(Value::Pointer(i)),
        Instruction::DeclareLetMut,
        // loop:
        // if i == 30 jump to end
        Instruction::Push(Value::Pointer(i)),
        Instruction::Ref,
        Instruction::Push(Value::I64(amount)),
        Instruction::Compare,
        Instruction::Push(Value::I8(0)),
        Instruction::Push(Value::Pointer(end_label)),
        Instruction::JumpIfEqual,
        // i += 1
        Instruction::Push(Value::Pointer(i)),
        Instruction::Ref,
        Instruction::Push(Value::I64(1)),
        Instruction::Add,
        Instruction::Push(Value::Pointer(i)),
        Instruction::Assign,
        // temp = a
        Instruction::Push(Value::Pointer(a)),
        Instruction::Clone,
        Instruction::Push(Value::Pointer(temp)),
        Instruction::Assign,
        // a = b
        Instruction::Push(Value::Pointer(b)),
        Instruction::Clone,
        Instruction::Push(Value::Pointer(a)),
        Instruction::Assign,
        // b = b + temp
        Instruction::Push(Value::Pointer(b)),
        Instruction::Ref,
        Instruction::Push(Value::Pointer(temp)),
        Instruction::Ref,
        Instruction::Add,
        Instruction::Push(Value::Pointer(b)),
        Instruction::Assign,
        // goto loop
        Instruction::Push(Value::Pointer(loop_label)),
        Instruction::Jump,
        // end:
        // print b
        Instruction::Push(Value::Pointer(b)),
        Instruction::Ref,
        Instruction::Print,
    ]);

    vm.run().unwrap();
}

struct VM<'stack> {
    interner: Interner,

    variables: Vec<Rc<Variable>>, // <StringId, Variable>
    labels: Vec<usize>,           // <StringId, InstructionIndex>

    ip: usize,
    program: Vec<Instruction>,
    stack: Stack<'stack, Value>,
}

impl Default for VM<'_> {
    fn default() -> Self {
        Self {
            interner: Interner::default(),
            variables: Vec::new(),
            labels: Vec::new(),
            ip: 0,
            program: Vec::new(),
            stack: Stack::default(),
        }
    }
}

macro_rules! deref {
    (cloned; $value: expr) => {{
        match $value {
            v @ Value::Reference(_) | v @ Value::MutableReference(_) => v.inside_cloned(),
            v => v,
        }
    }};
    (ref; $value: expr) => {
        match &$value {
            v @ Value::Reference(_) | v @ Value::MutableReference(_) => v.inside_ref(),
            ref v => v,
        }
    };
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
                    let variable = self.variables[pointer.as_usize()].clone();
                    self.stack.push(Value::Reference(variable))
                }
                Instruction::Clone => {
                    let pointer = self.stack.pop();
                    let variable = &self.variables[pointer.as_usize()];
                    self.stack.push(variable.inside_cloned());
                }

                Instruction::DeclareLet => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();

                    self.push_variable(
                        name.as_usize(),
                        Variable::Immutable(ImmutableVariable(value)),
                    );
                }
                Instruction::DeclareLetMut => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();

                    self.push_variable(
                        name.as_usize(),
                        Variable::Mutable(MutableVariable(value.into())),
                    );
                }

                Instruction::Assign => {
                    let pointer = self.stack.pop();
                    let value = self.stack.pop();

                    let variable = self.get_variable(pointer.as_usize());
                    let Variable::Mutable(variable) = variable else {
                        panic!("Attempted to assign to an immutable variable.")
                    };

                    unsafe {
                        variable.set(value);
                    }
                }

                Instruction::Add => binary_operation!(self, +),
                Instruction::Subtract => binary_operation!(self, -),
                Instruction::Multiply => binary_operation!(self, *),
                Instruction::Divide => binary_operation!(self, /),
                Instruction::Modulo => binary_operation!(self, %),

                Instruction::Jump => {
                    let pointer = self.stack.pop();
                    self.jump_interned(pointer.as_usize());
                }
                Instruction::JumpAbsolute => {
                    let ip = self.stack.pop();
                    self.jump_abs(ip.into());
                }
                Instruction::JumpName => {
                    let name = self.stack.pop();
                    self.jump_name(name.as_string());
                }

                Instruction::JumpIfEqual => {
                    let interned_label = self.stack.pop();

                    let a = deref!(cloned; self.stack.pop());
                    let b = deref!(cloned; self.stack.pop());

                    if a == b {
                        self.jump_interned(interned_label.into());
                    }
                }

                Instruction::Compare => {
                    let b = self.stack.pop();
                    let b = deref!(ref; b);
                    let a = deref!(cloned; self.stack.pop());

                    let cmp = a.partial_cmp(b).unwrap();

                    self.stack.push(Value::I8(cmp as i8));
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
        if self.variables.len() <= interned {
            self.variables
                .resize(interned + 1, Rc::new(Variable::default()));
        }
        self.variables[interned] = Rc::new(variable);
        interned
    }

    pub fn get_variable(&self, interned: StringID) -> &Variable {
        self.variables
            .get(interned)
            .expect("Attempted to get a variable that does not exist.")
    }

    pub fn label_name(&mut self, name: &str, ip: usize) -> StringID {
        let interned = self.interner.intern(name);
        if self.labels.len() <= interned {
            self.labels.resize(interned + 1, 0);
        }
        self.labels[interned] = ip;
        interned
    }

    pub fn jump_abs(&mut self, ip: usize) {
        self.ip = ip;
    }

    pub fn jump_interned(&mut self, interned: StringID) {
        self.ip = self.labels[interned];
    }

    pub fn jump_name(&mut self, name: &str) {
        let interned = self.interner.intern(name);
        self.ip = self.labels[interned];
    }
}
