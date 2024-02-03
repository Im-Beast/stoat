use lexer::{Token, TokenKind};

use miette::{bail, Error, Result};

#[repr(u8)]
#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,

    InclusiveRange,
    ExclusiveRange,

    And,
    Nand,
    Or,
    Nor,
    Not,

    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    Reference,
    MutableReference,
}

impl TryFrom<&Token> for Operator {
    type Error = Error;

    fn try_from(value: &Token) -> Result<Self> {
        (&value.0).try_into()
    }
}

impl TryFrom<&TokenKind> for Operator {
    type Error = Error;

    fn try_from(value: &TokenKind) -> Result<Self> {
        let operator = match value {
            TokenKind::Add => Operator::Add,
            TokenKind::Subtract => Operator::Subtract,
            TokenKind::Multiply => Operator::Multiply,
            TokenKind::Divide => Operator::Divide,
            TokenKind::Modulo => Operator::Modulo,

            TokenKind::Assign => Operator::Assign,
            TokenKind::AddAssign => Operator::AddAssign,
            TokenKind::SubtractAssign => Operator::SubtractAssign,
            TokenKind::MultiplyAssign => Operator::MultiplyAssign,
            TokenKind::DivideAssign => Operator::DivideAssign,
            TokenKind::ModuloAssign => Operator::ModuloAssign,

            TokenKind::And => Operator::And,
            TokenKind::Nand => Operator::Nand,
            TokenKind::Or => Operator::Or,
            TokenKind::Nor => Operator::Nor,
            TokenKind::Not => Operator::Not,

            TokenKind::Equals => Operator::Equal,
            TokenKind::NotEquals => Operator::NotEqual,
            TokenKind::LessThan => Operator::LessThan,
            TokenKind::LessThanOrEqual => Operator::LessThanOrEqual,
            TokenKind::GreaterThan => Operator::GreaterThan,
            TokenKind::GreaterThanOrEqual => Operator::GreaterThanOrEqual,

            TokenKind::DoubleDot => Operator::ExclusiveRange,
            TokenKind::DoubleDotEquals => Operator::ExclusiveRange,

            token => bail!("Cannot convert {token:?} into an operator"),
        };

        Ok(operator)
    }
}
