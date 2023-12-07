use crate::shared::name_bihasher::NameHash;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(NameHash), // x

    Wildcard,              // _
    Rest,                  // ..
    Mutable(Box<Pattern>), // mut

    Tuple(Vec<Pattern>), // (a, b, .., z)
    Array(Vec<Pattern>), // [a, b, .., z]
}
