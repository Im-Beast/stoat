use std::borrow::Cow;

use miette::{bail, Result};
use shared::interner::InternedString;

pub mod stack;
use stack::Stack;

pub mod call_frame;
use call_frame::CallFrame;

pub mod instruction;
use instruction::Instruction;

pub mod value;
use value::Value;

pub mod program;
use program::Program;

macro_rules! binary_operation {
    ($self: expr, $operand: tt) => {{
        let a = $self.stack.pop();
        let b = $self.stack.pop();

        match (a.as_ref(), b.as_ref()) {
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

            (a, b) => panic!("Cannot perform binary operation on non-numeric and/or mixed values. Got {a:?} and {b:?}"),
        }
    }}
}

#[derive(Debug)]
struct VM<'stack> {
    labels: Vec<usize>, // InternedString -> InstructionPointer

    call_stack: Vec<CallFrame>,

    program: Program,
    stack: Stack<'stack, Value>,
    ip: usize,
}

impl<'stack> VM<'stack> {
    pub fn new(program: Program) -> Self {
        Self {
            labels: Vec::new(),

            call_stack: Vec::from([CallFrame::new(program.len())]),

            program,
            stack: Stack::default(),
            ip: 0,
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
                    let pointer = self.stack.pop().as_usize();
                    let value = self.get_variable_deep(pointer).expect("Variable not found");
                    self.stack.push_ref(value)
                }
                Instruction::Clone => {
                    let pointer = self.stack.pop().as_usize();
                    let value = self.get_variable_deep(pointer).expect("Variable not found");
                    self.stack.push(value.clone());
                }
                Instruction::Duplicate => {
                    let value = self.stack.pop();
                    let cloned = value.as_ref().clone();
                    self.stack.push_cow(value);
                    self.stack.push(cloned);
                }

                Instruction::DeclareVariable => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();
                    self.push_variable(name.as_usize(), value);
                }

                Instruction::Assign => {
                    let name = self.stack.pop();
                    let value = self.stack.pop();

                    let ip = *self
                        .current_frame_mut()
                        .get_variable_pointer(name.as_usize())
                        .expect("Variable not found");

                    let variable = self.stack.get_mut(ip);
                    *variable = value;
                }

                Instruction::Add => binary_operation!(self, +),
                Instruction::Subtract => binary_operation!(self, -),
                Instruction::Multiply => binary_operation!(self, *),
                Instruction::Divide => binary_operation!(self, /),
                Instruction::Modulo => binary_operation!(self, %),

                Instruction::Jump => {
                    let label = self.stack.pop();
                    self.jump(label.as_usize());
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

                Instruction::Call => {
                    let label = self.stack.pop();
                    self.call(label.as_usize());
                }
                Instruction::Return => {
                    let frame = self.call_stack.pop().expect("Call stack is empty");
                    self.ip = frame.return_pointer;
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

    pub fn current_frame(&self) -> &CallFrame {
        self.call_stack.last().expect("Call stack is empty")
    }

    pub fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().expect("Call stack is empty")
    }

    pub fn call(&mut self, label: InternedString) {
        self.call_stack.push(CallFrame::new(self.ip));
        self.jump(label);
    }

    pub fn push_variable(&mut self, name: InternedString, value: Cow<'stack, Value>) {
        let len = self.stack.len();
        let frame = self.current_frame_mut();
        frame.set_variable_pointer(name, len);
        self.stack.push_cow(value);
    }

    pub fn get_variable_deep(&self, name: InternedString) -> Option<&'stack Value> {
        for frame in self.call_stack.iter().rev() {
            if let Some(value) = self.get_variable_from(frame, name) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_variable_from(
        &self,
        frame: &CallFrame,
        name: InternedString,
    ) -> Option<&'stack Value> {
        let ip = frame.get_variable_pointer(name);

        let ip = match ip {
            Some(ip) => *ip,
            None => return None,
        };

        // SAFETY: This is safe for a properly constructed program
        // where all variables are declared before they are used
        // and are not popped from the stack.
        unsafe {
            let value = match self.stack.get(ip) {
                Cow::Borrowed(value) => value,
                Cow::Owned(value) => value,
            };
            Some(std::mem::transmute_copy::<&Value, &'stack Value>(&value))
        }
    }

    pub fn label(&mut self, label: InternedString, ip: usize) {
        if self.labels.len() <= label {
            self.labels.resize(label + 1, 0);
        }
        self.labels[label] = ip;
    }

    pub fn jump(&mut self, label: InternedString) {
        self.ip = self.labels[label];
    }

    pub fn jump_abs(&mut self, ip: usize) {
        self.ip = ip;
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use shared::interner::Interner;

    #[test]
    fn fibonacci() {
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
                // fibonacci sequence till amount–th number
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
                // if i == amount jump to end
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
                // &b
                Instruction::Push(Value::Pointer(b)),
                Instruction::Ref,
            ],
        );

        let mut vm = VM::new(program);

        vm.label(fib, 12);
        vm.label(end, 42);

        vm.run().unwrap();

        assert_eq!(vm.stack.pop().as_ref(), &Value::I64(7540113804746346429));
    }

    #[test]
    fn function_calls() {
        let mut interner = Interner::default();

        let a = interner.intern("a");
        let b = interner.intern("b");
        let c = interner.intern("c");
        let multiply = interner.intern("multiply");
        let square = interner.intern("square");

        let program = Program::new(
            interner,
            vec![
                // a = 5
                // b = 3
                // c = multiply(a, b)
                // c = square(c)
                // print(c)

                // a = 5
                Instruction::Push(Value::I64(5)),
                Instruction::Push(Value::Pointer(a)),
                Instruction::DeclareVariable,
                // b = 3
                Instruction::Push(Value::I64(3)),
                Instruction::Push(Value::Pointer(b)),
                Instruction::DeclareVariable,
                // c = multiply(a, b)
                Instruction::Push(Value::Pointer(a)),
                Instruction::Clone,
                Instruction::Push(Value::Pointer(b)),
                Instruction::Ref,
                Instruction::Push(Value::Pointer(multiply)), // label
                Instruction::Call,
                Instruction::Push(Value::Pointer(c)),
                Instruction::DeclareVariable,
                // c = square(c)
                Instruction::Push(Value::Pointer(c)),
                Instruction::Ref,
                Instruction::Push(Value::Pointer(square)), // label
                Instruction::Call,
                Instruction::Push(Value::Pointer(c)),
                Instruction::Assign,
                // &c
                Instruction::Push(Value::Pointer(c)),
                Instruction::Ref,
                // break out of main
                Instruction::Return,
                // multiply(a, b):
                // return a * b
                Instruction::Multiply,
                Instruction::Return,
                // square(a):
                // return a * a
                Instruction::Duplicate,
                Instruction::Multiply,
                Instruction::Return,
            ],
        );

        let mut vm = VM::new(program);

        vm.label(multiply, 23);
        vm.label(square, 25);

        vm.run().unwrap();

        assert_eq!(vm.stack.pop().as_ref(), &Value::I64(225));
    }
}
