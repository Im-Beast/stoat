use std::borrow::Cow;

#[derive(Debug, Clone)]
pub struct Stack<'a, T: Clone> {
    stack: Vec<Cow<'a, T>>,
}

impl<T: Clone> Default for Stack<'_, T> {
    fn default() -> Self {
        Self { stack: Vec::new() }
    }
}

impl<'a, T: Clone> Stack<'a, T> {
    pub fn push(&mut self, value: T) {
        self.stack.push(Cow::Owned(value));
    }

    pub fn push_ref(&mut self, value: &'a T) {
        self.stack.push(Cow::Borrowed(value));
    }

    pub fn pop(&mut self) -> T {
        self.stack
            .pop()
            .expect("Cannot pop from empty stack")
            .into_owned()
    }
}
