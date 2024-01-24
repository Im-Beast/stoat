use shared::interner::InternedString;
use vm::value::Value;

use crate::{Operator, Statement};

#[derive(Debug)]
#[repr(u8)]
pub enum Expression {
    Value(Value),
    VariableAccess(VariableAccess),
    PropertyAccess(PropertyAccess),
    BinaryOperation(BinaryOperation),
    Call(Call),
    Block(Block),
    If(If),
}

#[derive(Debug)]
#[repr(transparent)]
pub struct VariableAccess {
    pub identifier: InternedString,
}

#[derive(Debug)]
pub struct PropertyAccess {
    pub object: Box<Expression>,
    pub property: InternedString,
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct Call {
    pub identifier: InternedString,
    pub arguments: Option<Box<[Expression]>>,
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Block {
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Expression>,
    pub then_block: Block,
    pub else_block: Option<Box<Expression>>, // either block or if
}