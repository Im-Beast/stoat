use lexer::{NumberPrefix, NumberSuffix};
use shared::interner::InternedString;
use vm::value::Value;

use crate::{value_type::Type, Operator, Statement};

#[repr(u8)]
pub enum ExpressionContext {
    // Required semicolons after expressions
    Default,
    // Expression without semicolon afterwards
    NoSemicolon,
    IfCondition,
    // Expression without semicolon afterwards,
    // lack of it signals implicit return
    Block,
    Function,
}

#[derive(Debug)]
#[repr(u8)]
pub enum Expression {
    Value(Value),
    UnknownNumber(Option<NumberPrefix>, NumberSuffix, String),
    VariableAccess(VariableAccess),
    PropertyAccess(PropertyAccess),
    BinaryOperation(BinaryOperation),
    Call(Call),
    Block(Block),
    If(If),
    Function(Function),
    ImplicitReturn(Return),
    ExplicitReturn(Return),
}

#[derive(Debug)]
#[repr(transparent)]
pub struct VariableAccess {
    pub identifier: InternedString,
}

#[derive(Debug)]
pub struct PropertyAccess {
    pub expression: Box<Expression>,
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
    pub object: Box<Expression>,
    pub arguments: Option<Box<[Expression]>>,
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Block {
    pub body: Box<[Statement]>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Expression>,
    pub then_block: Block,
    pub else_block: Option<Box<Expression>>, // either block or if
}

#[derive(Debug)]
pub struct Function {
    pub identifier: InternedString,
    pub parameters: Option<Box<[(InternedString, Type)]>>,
    pub return_type: Option<Type>,
    pub block: Block,
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Return {
    pub expression: Box<Expression>,
}
