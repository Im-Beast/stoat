use std::borrow::Cow;

use crate::shared::interner::{InternedString, Interner};
use miette::{bail, Result};

mod stack;
use stack::Stack;

mod instruction;
use instruction::Instruction;

mod value;
use value::Value;

mod program;
use program::Program;

pub fn vm_test() {
    let mut interner = Interner::default();

    let a = interner.intern("a");
    let b = interner.intern("b");
    let i = interner.intern("i");
    let temp = interner.intern("temp");

    let fib = interner.intern("loop");
    let end = interner.intern("end");

    let amount = 91;

    let program = Program::new(
        interner,
        vec![
            // fibonacci sequence till 30th number
            // a = 0
            Instruction::Push(Value::I64(0)),
            Instruction::Push(Value::Pointer(a)),
            Instruction::DeclareVariable,
            // b = 1
            Instruction::Push(Value::I64(1)),
            Instruction::Push(Value::Pointer(b)),
            Instruction::DeclareVariable,
            // i = 0
            Instruction::Push(Value::I64(0)),
            Instruction::Push(Value::Pointer(i)),
            Instruction::DeclareVariable,
            // temp = 0
            Instruction::Push(Value::I64(0)),
            Instruction::Push(Value::Pointer(temp)),
            Instruction::DeclareVariable,
            // loop:
            // if i == 30 jump to end
            Instruction::Push(Value::Pointer(i)),
            Instruction::Ref,
            Instruction::Push(Value::I64(amount)),
            Instruction::Compare,
            Instruction::Push(Value::I8(0)),
            Instruction::Push(Value::Pointer(end)),
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
            Instruction::Push(Value::Pointer(fib)),
            Instruction::Jump,
            // end:
            // print b
            Instruction::Push(Value::Pointer(b)),
            Instruction::Ref,
            Instruction::Print,
        ],
    );

    let mut vm = VM::new(program);

    vm.label(fib, 12);
    vm.label(end, 42);

    vm.run().unwrap();
}

macro_rules! binary_operation {
    ($self: expr, $operand: tt) => {{
        let a = $self.stack.pop();
        let b = $self.stack.pop();

        match (a.as_ref(), b.as_ref()) {
            (Value::I8(a), Value::I8(b))   => $self.stack.push(Value::I8(b $operand a)),
            (Value::I16(a), Value::I16(b)) => $self.stack.push(Value::I16(b $operand a)),
            (Value::I32(a), Value::I32(b)) => $self.stack.push(Value::I32(b $operand a)),
            (Value::I64(a), Value::I64(b)) => $self.stack.push(Value::I64(b $operand a)),

            (Value::U8(a), Value::U8(b))   => $self.stack.push(Value::U8(b $operand a)),
            (Value::U16(a), Value::U16(b)) => $self.stack.push(Value::U16(b $operand a)),
            (Value::U32(a), Value::U32(b)) => $self.stack.push(Value::U32(b $operand a)),
            (Value::U64(a), Value::U64(b)) => $self.stack.push(Value::U64(b $operand a)),

            (Value::F32(a), Value::F32(b)) => $self.stack.push(Value::F32(b $operand a)),
            (Value::F64(a), Value::F64(b)) => $self.stack.push(Value::F64(b $operand a)),

            (a, b) => panic!("Cannot perform binary operation on non-numeric and mixed values. Got {a:?} and {b:?}"),
        }
    }}
}

// TODO: Calls
#[derive(Debug, Clone, Copy)]
struct CallFrame {
    arity: u8,
    return_pointer: usize,
}

impl CallFrame {
    pub fn new(arity: u8, return_pointer: usize) -> Self {
        Self {
            arity,
            return_pointer,
        }
    }
}

struct VM<'stack> {
    program: Program,

    variables: Vec<InternedString>, // InternedString -> InstructionIndex
    labels: Vec<InternedString>,    // InternedString -> InstructionIndex

    ip: usize,
    call_stack: Stack<'stack, CallFrame>,
    stack: Stack<'stack, Value>,
}

impl<'stack> VM<'stack> {
    pub fn new(program: Program) -> Self {
        Self {
            variables: Vec::new(),
            labels: Vec::new(),

            ip: 0,
            stack: Stack::default(),
            call_stack: Stack::from([CallFrame::new(0, program.len())]),

            program,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        if self.ip >= self.program.len() {
            bail!("Attempted to run an empty program.")
        }

        while self.ip < self.program.len() {
            let instruction = self.program.get(self.ip);
            self.ip += 1;

            match instruction {
                Instruction::Push(value) => self.stack.push(value.to_owned()),
                Instruction::Pop => {
                    self.stack.pop();
                }

                Instruction::Ref => {
                    let pointer = self.stack.pop();
                    let pointer = pointer.as_usize();
                    let value = self.get_variable(pointer);
                    self.stack.push_ref(value)
                }
                Instruction::Clone => {
                    let pointer = self.stack.pop();
                    let value = self.get_variable(pointer.as_usize());
                    self.stack.push(value.clone());
                }

                Instruction::DeclareVariable => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();
                    self.push_variable(name.as_usize(), value);
                }

                Instruction::Assign => {
                    let pointer = self.stack.pop();
                    let value = self.stack.pop();

                    let ip = self.variables[pointer.as_usize()];

                    let variable = self.stack.get_mut(ip);
                    *variable = value;
                }

                Instruction::Add => binary_operation!(self, +),
                Instruction::Subtract => binary_operation!(self, -),
                Instruction::Multiply => binary_operation!(self, *),
                Instruction::Divide => binary_operation!(self, /),
                Instruction::Modulo => binary_operation!(self, %),

                Instruction::Jump => {
                    let pointer = self.stack.pop();
                    self.jump(pointer.as_usize());
                }
                Instruction::JumpAbsolute => {
                    let ip = self.stack.pop();
                    self.jump_abs(ip.as_usize());
                }

                Instruction::JumpIfEqual => {
                    let interned_label = self.stack.pop();

                    let a = self.stack.pop();
                    let b = self.stack.pop();

                    if a == b {
                        self.jump(interned_label.as_usize());
                    }
                }

                Instruction::Compare => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    let cmp = a.partial_cmp(&b).unwrap();

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

    pub fn push_variable(&mut self, interned: usize, value: Cow<'stack, Value>) -> usize {
        // Make sure we have enough space
        if self.variables.len() <= interned {
            self.variables.resize(interned + 1, usize::MAX);
        }
        self.variables[interned] = self.stack.len();
        self.stack.push_cow(value);
        interned
    }

    pub fn get_variable(&self, interned: usize) -> &'stack Value {
        let ip = self.variables[interned];
        // SAFETY: This is safe for a properly constructed program
        // where all variables are declared before they are used
        // and are not popped from the stack.
        unsafe {
            let value = match self.stack.get(ip) {
                Cow::Borrowed(value) => value,
                Cow::Owned(value) => value,
            };
            std::mem::transmute::<&'_ Value, &'stack Value>(value)
        }
    }

    pub fn label(&mut self, interned: usize, ip: usize) {
        if self.labels.len() <= interned {
            self.labels.resize(interned + 1, 0);
        }
        self.labels[interned] = ip;
    }

    pub fn jump_abs(&mut self, ip: usize) {
        self.ip = ip;
    }

    pub fn jump(&mut self, interned: usize) {
        self.ip = self.labels[interned];
    }
}
