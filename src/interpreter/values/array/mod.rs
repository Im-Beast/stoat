use std::borrow::Cow;

use super::Value;

mod into_iter;

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayValue<'a, 'b>(pub Box<[Cow<'b, Value<'a>>]>, pub usize);
