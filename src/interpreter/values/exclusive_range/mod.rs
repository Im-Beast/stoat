use super::Value;

mod into_iter;

#[derive(Debug, Clone, PartialEq)]
pub struct ExclusiveRangeValue<'a>(pub Box<Value<'a>>, pub Box<Value<'a>>); // from to
