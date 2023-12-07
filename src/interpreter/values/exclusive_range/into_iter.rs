use std::borrow::Cow;

use crate::interpreter::values::Value;

use super::ExclusiveRangeValue;

pub struct ExclusiveRangeValueIntoIter<'a, 'b>(
    usize,
    Value<'a>,     // (from at the start), value will be modified between runs
    &'b Value<'a>, // to
); // ptr, value_type, value

impl<'a, 'b> IntoIterator for &'b ExclusiveRangeValue<'a> {
    type Item = Cow<'b, Value<'a>>;
    type IntoIter = ExclusiveRangeValueIntoIter<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        ExclusiveRangeValueIntoIter(0, (*self.0).clone(), &self.1)
    }
}

impl<'a, 'b> Iterator for ExclusiveRangeValueIntoIter<'a, 'b> {
    type Item = Cow<'b, Value<'a>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        todo!("exclusive range iterator")
    }
}
