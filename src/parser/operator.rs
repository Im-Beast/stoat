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

            token => bail!("Cannot convert {token:?} into an operator"),
        };

        Ok(operator)
    }
}
