use std::borrow::Cow;

use crate::interpreter::values::Value;

use super::SliceValue;

pub struct SliceValueIntoIter<'a, 'b>(usize, &'b SliceValue<'a, 'b>); // ptr, value
impl<'a, 'b> IntoIterator for &'b SliceValue<'a, 'b> {
    type Item = Cow<'b, Value<'a>>;
    type IntoIter = SliceValueIntoIter<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        SliceValueIntoIter(0, self)
    }
}

impl<'a, 'b> Iterator for SliceValueIntoIter<'a, 'b> {
    type Item = Cow<'b, Value<'a>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let SliceValue(items, ..) = self.1;
        let result = items.get(self.0);
        self.0 += 1;

        if let Some(result) = result {
            Some(Cow::Borrowed(result))
        } else {
            None
        }
    }
}
