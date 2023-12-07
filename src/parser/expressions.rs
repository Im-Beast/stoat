use miette::{bail, Error, SourceSpan};

use crate::{lexer::Token, shared::name_bihasher::NameHash};

use super::statements::{Statement, StatementBody};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathOperation {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
}

impl<'a> TryFrom<&Token<'a>> for MathOperation {
    type Error = Error;
    fn try_from<'t>(token: &'t Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Add => MathOperation::Add,
            Token::AddAssign => MathOperation::Add,
            Token::Subtract => MathOperation::Subtract,
            Token::SubtractAssign => MathOperation::Subtract,
            Token::Multiply => MathOperation::Multiply,
            Token::MutliplyAssign => MathOperation::Multiply,
            Token::Divide => MathOperation::Divide,
            Token::DivideAssign => MathOperation::Divide,
            Token::Modulo => MathOperation::Modulo,
            Token::ModuloAssign => MathOperation::Modulo,
            _ => bail!("Cannot convert {token:?} into MathOperation"),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalComparison {
    And,
    Or,
    Nor,
    Nand,
}

impl<'a> TryFrom<&Token<'a>> for LogicalComparison {
    type Error = Error;
    fn try_from<'t>(token: &'t Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::And => LogicalComparison::And,
            Token::Or => LogicalComparison::Or,
            Token::Nor => LogicalComparison::Nor,
            Token::Nand => LogicalComparison::Nand,
            _ => bail!("Cannot convert {token:?} into LogicalComparison"),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrderComparison {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

impl<'a> TryFrom<&Token<'a>> for OrderComparison {
    type Error = Error;
    fn try_from<'t>(token: &'t Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Equal => OrderComparison::Equal,
            Token::NotEqual => OrderComparison::NotEqual,
            Token::LessThan => OrderComparison::LessThan,
            Token::LessThanOrEqual => OrderComparison::LessThanOrEqual,
            Token::GreaterThan => OrderComparison::GreaterThan,
            Token::GreaterThanOrEqual => OrderComparison::GreaterThanOrEqual,
            _ => bail!("Cannot convert {token:?} into OrderComparison"),
        })
    }
}

#[derive(Debug)]
pub struct ExpressionSpan<'a> {
    pub expression: Expression<'a>,
    pub from: usize,
    pub to: usize,
}

impl From<&ExpressionSpan<'_>> for SourceSpan {
    fn from(span: &ExpressionSpan) -> Self {
        (span.from..span.to).into()
    }
}

impl<'a> ExpressionSpan<'a> {
    pub fn new(expression: Expression<'a>, from: usize, to: usize) -> Self {
        Self {
            expression,
            from,
            to,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Integer32(i32),
    Integer64(i64),
    Float32(f32),
    Float64(f64),

    Boolean(bool),
    Function(Box<Statement<'a>>),
    String(&'a str),
    Array(Box<[Expression<'a>]>),
    Slice(Box<[Expression<'a>]>),
    Tuple(Box<[Expression<'a>]>),

    AccessProperty(Box<Expression<'a>>, NameHash), // item, property
    AccessIndex(Box<Expression<'a>>, Box<Expression<'a>>), // item, index

    VariableReference(NameHash),                       // identifier
    VariableAssignment(NameHash, Box<Expression<'a>>), // identifier, value

    FunctionCall(FunctionCallExpression<'a>),

    ContainedExpression(Box<Expression<'a>>), // (expression)

    MathOperation(MathOperationExpression<'a>),

    LogicalComparison(LogicalComparisonExpression<'a>),
    OrderComparison(OrderComparisonExpression<'a>),

    ExclusiveRange(Box<Expression<'a>>, Box<Expression<'a>>), // from, to
    InclusiveRange(Box<Expression<'a>>, Box<Expression<'a>>), // from, to

    Negate(Box<Expression<'a>>), // expression to be negated (! at the start)
    QuickBail(Box<Expression<'a>>), // expression to be bailed on (? at the end)
    QuickPanic(Box<Expression<'a>>), // expression to be panicked on (! at the end)

    Block(BlockExpression<'a>), // block

    If(IfExpression<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MathOperationExpression<'a>(
    pub Box<Expression<'a>>, // left_side
    pub Box<Expression<'a>>, // right_side
    pub MathOperation,       // operation
);

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalComparisonExpression<'a>(
    pub Box<Expression<'a>>, // left_side
    pub Box<Expression<'a>>, // right_side
    pub LogicalComparison,   // comparison
);

#[derive(Debug, Clone, PartialEq)]
pub struct OrderComparisonExpression<'a>(
    pub Box<Expression<'a>>, // left_side
    pub Box<Expression<'a>>, // right_side
    pub OrderComparison,     // comparison
);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallExpression<'a>(
    pub Box<Expression<'a>>,   // function
    pub Box<[Expression<'a>]>, // arguments
);

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpression<'a>(pub StatementBody<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression<'a>(
    pub Box<Expression<'a>>,         // condition
    pub BlockExpression<'a>,         // block
    pub Option<Box<Expression<'a>>>, // else_block (either if_expression or block_expression)
);
