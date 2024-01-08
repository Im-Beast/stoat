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

impl<'stack, T: Clone, const N: usize> From<[T; N]> for Stack<'stack, T> {
    fn from(array: [T; N]) -> Self {
        Self {
            stack: array.into_iter().map(|value| Cow::Owned(value)).collect(),
        }
    }
}

impl<'a, T: Clone> Stack<'a, T> {
    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn get(&self, n: usize) -> &Cow<'a, T> {
        &self.stack[n]
    }

    pub fn get_mut(&mut self, n: usize) -> &mut Cow<'a, T> {
        &mut self.stack[n]
    }

    pub fn push_cow(&mut self, value: Cow<'a, T>) {
        self.stack.push(value);
    }

    pub fn push(&mut self, value: T) {
        self.stack.push(Cow::Owned(value));
    }

    pub fn push_ref(&mut self, value: &'a T) {
        self.stack.push(Cow::Borrowed(value));
    }

    pub fn pop(&mut self) -> Cow<'a, T> {
        self.stack.pop().expect("Cannot pop from empty stack")
    }
}
