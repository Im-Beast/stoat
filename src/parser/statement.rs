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
    pub name: InternedString,
    pub value: Expression,
}
