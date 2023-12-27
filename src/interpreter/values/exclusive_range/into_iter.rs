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
        match self.1 {
            Value::Integer32(ref mut value) => {
                let Value::Integer32(to) = self.2 else {
                    unreachable!()
                };

                if *value < *to {
                    let result = Cow::Owned(Value::Integer32(*value));
                    *value += 1;
                    Some(result)
                } else {
                    None
                }
            }
            Value::Integer64(ref mut value) => {
                let Value::Integer64(to) = self.2 else {
                    unreachable!()
                };

                if *value < *to {
                    let result = Cow::Owned(Value::Integer64(*value));
                    *value += 1;
                    Some(result)
                } else {
                    None
                }
            }
            Value::Float32(ref mut value) => {
                let Value::Float32(to) = self.2 else {
                    unreachable!()
                };

                if *value < *to {
                    let result = Cow::Owned(Value::Float32(*value));
                    *value += 1.0;
                    Some(result)
                } else {
                    None
                }
            }
            Value::Float64(ref mut value) => {
                let Value::Float64(to) = self.2 else {
                    unreachable!()
                };

                if *value < *to {
                    let result = Cow::Owned(Value::Float64(*value));
                    *value += 1.0;
                    Some(result)
                } else {
                    None
                }
            }
            _ => unimplemented!(),
        }
    }
}
