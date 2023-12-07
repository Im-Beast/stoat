use std::borrow::Cow;

use crate::interpreter::values::Value;

use super::ArrayValue;

pub struct ArrayValueIntoIter<'a, 'b>(usize, &'b ArrayValue<'a, 'b>); // ptr, value
impl<'a, 'b> IntoIterator for &'b ArrayValue<'a, 'b> {
    type Item = Cow<'b, Value<'a>>;
    type IntoIter = ArrayValueIntoIter<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        ArrayValueIntoIter(0, self)
    }
}

impl<'a, 'b> Iterator for ArrayValueIntoIter<'a, 'b> {
    type Item = Cow<'b, Value<'a>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let ArrayValue(items, ..) = self.1;
        let result = items.get(self.0);
        if let Some(result) = result {
            self.0 += 1;
            Some(Cow::Borrowed(result))
        } else {
            None
        }
    }
}
