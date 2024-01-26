use crate::expression::Expression;
use shared::interner::InternedString;

#[derive(Debug)]
#[repr(u8)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub identifier: InternedString,
    pub mutable: bool,
    pub value_type: Type,
    pub value: Expression,
}
