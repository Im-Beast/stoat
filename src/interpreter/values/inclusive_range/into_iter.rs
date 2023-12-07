use std::borrow::Cow;

use crate::interpreter::values::Value;

use super::InclusiveRangeValue;

pub struct InclusiveRangeValueIntoIter<'a, 'b>(
    usize,
    Value<'a>,     // (from at the start), value will be modified between runs
    &'b Value<'a>, // to
); // ptr, value_type, value

impl<'a, 'b> IntoIterator for &'b InclusiveRangeValue<'a> {
    type Item = Cow<'b, Value<'a>>;
    type IntoIter = InclusiveRangeValueIntoIter<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        InclusiveRangeValueIntoIter(0, (*self.0).clone(), &self.1)
    }
}

impl<'a, 'b> Iterator for InclusiveRangeValueIntoIter<'a, 'b> {
    type Item = Cow<'b, Value<'a>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        todo!("inclusive range iterator")
    }
}
