mod into_iter;

use std::borrow::Cow;

use super::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct SliceValue<'a, 'b>(pub Vec<Cow<'b, Value<'a>>>);
