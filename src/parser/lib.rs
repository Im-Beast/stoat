use std::collections::VecDeque;

use miette::{Result, SourceSpan};

use expression::{ExpressionContext, VariableAccess};
use lexer::{NumberSuffix, Token, TokenKind};
use shared::{dbg_line, interner::Interner, span::Span};
use vm::value::Value;

mod operator;
pub use operator::Operator;

mod statement;
pub use statement::{Statement, VariableDeclaration};

mod expression;
pub use expression::{Block, Call, Expression, If, PropertyAccess};

mod ast;
pub use ast::AST;

mod error;
use error::{ParserError, UnexpectedEOF, UnexpectedToken};

mod value_type;
use value_type::Type;

use crate::{
    error::InvalidFloatSuffix,
    expression::{BinaryOperation, ContainedExpresison, Function, Return, UnaryOperation},
};

#[derive(Debug)]
pub struct ParserResult {
    pub ast: AST,
    pub errors: Vec<ParserError>,
}

pub struct Parser<'src> {
    code: &'src str,
    tokens: VecDeque<Token>,

    errors: Vec<ParserError>,

    interner: Interner,
    current_span: Span,
}

macro_rules! value_pattern {
    () => {
        TokenKind::String(_)
            | TokenKind::Char(_)
            | TokenKind::Bool(_)
            | TokenKind::Integer(..)
            | TokenKind::Float(..)
    };
}

macro_rules! no_semicolon_statement_pattern {
    () => {
        Statement::Expression(Expression::If(_) | Expression::Block(_))
    };
}

macro_rules! peek_from {
    ($self: expr, [$($pat: pat),*]) => {
        match $self.peek() {
            Some(token @ Token($($pat)|*, _span)) => token,
            Some(token) => {
                $self.unexpected_token(
                    dbg_line!(),
                    (&token.1).into(),
                    stringify!($($pat),*),
                    format!("{:?}", token.0)
                );
                return None;
            }
            None => {
                $self.unexpected_eof(stringify!($($pat),*));
                return None;
            }
        }
    };
}

macro_rules! consume_from {
    ($self: expr, [$($pat: pat),*]) => {
        match $self.consume_specific(stringify!($($pat),*)) {
            token @ Token($($pat)|*, _span) => token,
            token =>  {
                $self.unexpected_token(
                    dbg_line!(),
                    (&token.1).into(),
                    stringify!($($pat),*),
                    format!("{:?}", token.0)
                );
                return None
            },
        }
    };
}

macro_rules! consume {
    ($self: expr, $pat: pat) => {
        consume!($self, $pat => {})
    };

    ($self: expr, $pat: pat => $out: expr) => {
        match $self.consume_specific(stringify!($pat)) {
            Token($pat, _) => $out,
            token => {
                $self.unexpected_token(
                    dbg_line!(),
                    (&token.1).into(),
                    stringify!($pat),
                    format!("{:?}", token.0),
                );
                return None;
            }
          }
    };

    (span; $self: expr, $pat: pat, $out: tt) => {
        match $self.consume_specific(stringify!($pat)) {
            Token($pat, span) => ($out, span),
            token @ Token(.., _span) => {
                $self.unexpected_token(&token.1, stringify!($pat), format!("{:?}", token.0));
                return None;
            }
        }
    };
}

impl<'src> Parser<'src> {
    pub fn new(tokens: VecDeque<Token>, code: &'src str) -> Self {
        Self {
            code,

            current_span: tokens[0].1,
            errors: Vec::new(),

            interner: Interner::default(),

            tokens: tokens,
        }
    }

    pub fn error(&mut self, error: ParserError) {
        self.errors.push(error);
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    pub fn peek_next(&self, n: usize) -> Option<&Token> {
        self.tokens.get(n)
    }

    pub fn consume_specific(&mut self, message: &str) -> Token {
        match self.tokens.pop_front() {
            Some(token) => {
                self.current_span = token.1;
                token
            }
            None => {
                self.unexpected_eof(message);
                Token(TokenKind::Garbage(None), self.current_span)
            }
        }
    }

    pub fn consume_any(&mut self) -> Token {
        self.consume_specific("Any token")
    }

    pub fn unexpected_eof(&mut self, expected: &str) {
        self.error(ParserError::UnexpectedEOF(UnexpectedEOF {
            dbg_line: dbg_line!(),
            expected: expected.to_string(),
            src: self.code.to_string(),
            position: (&self.current_span).into(),
        }))
    }

    pub fn unexpected_token(
        &mut self,
        dbg_line: String,
        span: SourceSpan,
        expected: &str,
        actual: String,
    ) {
        self.error(ParserError::UnexpectedToken(UnexpectedToken {
            dbg_line: dbg_line,
            expected: expected.to_string(),
            actual: actual.to_string(),
            src: self.code.to_string(),
            position: span,
        }))
    }

    pub fn parse(mut self) -> ParserResult {
        let mut ast = AST::default();

        while let Some(statement) = self.parse_statement(&ExpressionContext::Default) {
            ast.body.push(statement)
        }

        ParserResult {
            ast,
            errors: self.errors,
        }
    }

    pub fn parse_expression_statement(&mut self, context: &ExpressionContext) -> Option<Statement> {
        let expression = self.parse_expression(context)?;
        Some(Statement::Expression(expression))
    }

    pub fn parse_expression(&mut self, context: &ExpressionContext) -> Option<Expression> {
        // Possible expressions:
        // value
        // variable_access
        // expr (op expr)*                                      (binary operation)
        // expr.ident                                           (property access)
        // if cond { ...stmt[]; expr? } else { ...stmt[]; expr? } (if expression)
        // { ...stmt[]; expr? }                                  (block)

        let mut expression_stack = Vec::new();

        expression_stack.push(self.parse_operand_with_unary_operators(context)?);

        loop {
            let Some(operator) = self.peek() else {
                match context {
                    ExpressionContext::NoSemicolon => {}
                    ExpressionContext::Default => self.unexpected_eof("Operator or Semicolon"),
                    ExpressionContext::IfCondition => self.unexpected_eof("Operator or LeftCurly"),
                    ExpressionContext::Function | ExpressionContext::Block => {
                        self.unexpected_eof("Operator, Semicolon or RightCurly")
                    }
                }
                break;
            };

            let Ok(operator): Result<Operator> = operator.try_into() else {
                break;
            };
            self.consume_any();

            let Some(lhs) = expression_stack.pop() else {
                // TODO: empty expression_stack => error
                println!("Empty expression stack !@#@!");
                return None;
            };

            // We don't want to parse the semicolon (or any delimiter for that matter)
            // in that part of expression, since we're handling it
            // at the end of the expression parsing step
            let rhs = self.parse_expression(&ExpressionContext::NoSemicolon)?;

            expression_stack.push(Expression::BinaryOperation(BinaryOperation {
                left: Box::new(lhs),
                operator,
                right: Box::new(rhs),
            }));
        }

        // TODO: expression_stack NOT empty => error
        let mut expression = expression_stack.pop().unwrap();

        match context {
            // Semicolon
            ExpressionContext::Default => consume!(self, TokenKind::Semicolon),
            // No semicolon
            ExpressionContext::NoSemicolon => {}
            // Left curly bracket
            ExpressionContext::IfCondition => {
                // ! When we parse if conditions, we don't consume the left bracket
                // ! Because parse_block_expression does it for us
                peek_from!(self, [TokenKind::LeftCurly]);
            }
            // Semicolon or imply return
            ExpressionContext::Function | ExpressionContext::Block => {
                // ! When we parse blocks, we don't consume the delimiter
                // ! It's handled by the block parsing function
                let token = peek_from!(self, [TokenKind::Semicolon, TokenKind::RightCurly]);
                if let TokenKind::RightCurly = token.0 {
                    expression = Expression::ImplicitReturn(Return {
                        expression: Box::new(expression),
                    });
                }
            }
        }

        Some(expression)
    }

    pub fn parse_operand(&mut self) -> Option<Expression> {
        // Possible operands:
        // value
        // variable_access
        // if cond { ...stmt[]; expr } else { ...stmt[]; expr }
        // operand.ident
        // operand

        let token = peek_from!(
            self,
            [TokenKind::Identifier(_), TokenKind::If, value_pattern!()]
        );

        let mut expression = match token.0 {
            TokenKind::If => self.parse_if_expression()?,
            TokenKind::Identifier(_) => self.parse_identifier_expression()?,
            value_pattern!() => self.parse_value_expression()?,
            _ => unreachable!(),
        };

        loop {
            match self.peek() {
                Some(Token(TokenKind::Dot, _)) => {
                    consume!(self, TokenKind::Dot);

                    let identifier = consume!(
                        self, TokenKind::Identifier(identifier) => identifier
                    );

                    let interned_ident = self.interner.intern(&identifier);

                    expression = Expression::PropertyAccess(PropertyAccess {
                        expression: Box::new(expression),
                        property: interned_ident,
                    });
                }

                Some(Token(TokenKind::LeftParen, _)) => {
                    consume!(self, TokenKind::LeftParen);

                    let arguments = if let TokenKind::RightParen = self.peek()?.0 {
                        self.consume_any();
                        None
                    } else {
                        let mut arguments = Vec::new();
                        loop {
                            arguments.push(self.parse_expression(&ExpressionContext::NoSemicolon)?);
                            let token =
                                consume_from!(self, [TokenKind::Comma, TokenKind::RightParen]);

                            if let TokenKind::RightParen = token.0 {
                                break;
                            }
                        }
                        Some(arguments.into_boxed_slice())
                    };

                    expression = Expression::Call(Call {
                        object: Box::new(expression),
                        arguments: arguments,
                    });
                }

                _ => break,
            }
        }

        Some(expression)
    }

    pub fn parse_operand_with_unary_operators(
        &mut self,
        context: &ExpressionContext,
    ) -> Option<Expression> {
        match self.peek()?.0 {
            TokenKind::Not => self.parse_not_expression(context),
            TokenKind::Return => self.parse_return_expression(context),
            TokenKind::LeftParen => self.parse_contained_expression(),
            _ => self.parse_operand(),
        }
    }

    pub fn parse_not_expression(&mut self, context: &ExpressionContext) -> Option<Expression> {
        consume!(self, TokenKind::Not);
        let expression = self.parse_expression(context)?;
        Some(Expression::UnaryOperation(UnaryOperation {
            operator: Operator::Not,
            expression: Box::new(expression),
        }))
    }

    pub fn parse_return_expression(&mut self, context: &ExpressionContext) -> Option<Expression> {
        consume!(self, TokenKind::Return);
        let expression = self.parse_expression(context)?;
        Some(Expression::ExplicitReturn(Return {
            expression: Box::new(expression),
        }))
    }

    pub fn parse_contained_expression(&mut self) -> Option<Expression> {
        consume!(self, TokenKind::LeftParen);
        let expression = self.parse_expression(&ExpressionContext::NoSemicolon)?;
        consume!(self, TokenKind::RightParen);
        Some(Expression::Contained(ContainedExpresison {
            expression: Box::new(expression),
        }))
    }

    pub fn parse_return_statement(&mut self, context: &ExpressionContext) -> Option<Statement> {
        let expression = self.parse_return_expression(context)?;
        Some(Statement::Expression(expression))
    }

    pub fn parse_identifier_expression(&mut self) -> Option<Expression> {
        // Possible expressions:
        // ident           (variable access)
        // ident((expr,)*) (function call)

        let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
        let interned_ident = self.interner.intern(&identifier);

        let token = self.peek();

        let expression = match token {
            _ => Expression::VariableAccess(VariableAccess {
                identifier: interned_ident,
            }),
        };

        Some(expression)
    }

    pub fn parse_if_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_if_expression()?;
        Some(Statement::Expression(expression))
    }

    pub fn parse_if_expression(&mut self) -> Option<Expression> {
        // if cond { stmt[]; expr } else { stmt[]; expr }

        consume!(self, TokenKind::If);

        let condition = self.parse_expression(&ExpressionContext::IfCondition)?;
        let then_block = self.parse_block_expression(&ExpressionContext::Block)?;
        let else_block = if matches!(self.peek(), Some(Token(TokenKind::Else, _))) {
            consume!(self, TokenKind::Else);

            let next = self.peek()?;
            Some(Box::new(match &next.0 {
                TokenKind::If => {
                    let expression = self.parse_if_expression()?;
                    expression
                }
                _ => {
                    let block = self.parse_block_expression(&ExpressionContext::Block)?;
                    Expression::Block(block)
                }
            }))
        } else {
            None
        };

        Some(Expression::If(If {
            condition: Box::new(condition),
            then_block,
            else_block,
        }))
    }

    pub fn parse_block_expression(&mut self, context: &ExpressionContext) -> Option<Block> {
        // { stmt[]; expr? };

        consume!(self, TokenKind::LeftCurly);

        // stmt[]; expr?
        let mut body = Vec::new();
        loop {
            if let TokenKind::RightCurly = self.peek()?.0 {
                break;
            }

            let statement = self.parse_statement(context)?;

            match statement {
                // We don't want to assert the type of the token
                // It was done already in parse_expression
                // We are sure that the next token is a right curly
                Statement::Expression(Expression::ImplicitReturn(_)) => break,

                // We don't require a semicolon after expressions like ifs
                no_semicolon_statement_pattern!() => {}

                _ => consume!(self, TokenKind::Semicolon),
            }

            body.push(statement);
        }

        consume!(self, TokenKind::RightCurly);

        Some(Block {
            body: body.into_boxed_slice(),
        })
    }

    pub fn parse_block_statement(&mut self) -> Option<Statement> {
        let block = self.parse_block_expression(&ExpressionContext::Block)?;
        Some(Statement::Expression(Expression::Block(block)))
    }

    pub fn parse_value_expression(&mut self) -> Option<Expression> {
        let token = peek_from!(self, [value_pattern!()]);

        let value = match &token.0 {
            TokenKind::Integer(..) => self.parse_integer()?,
            TokenKind::Float(..) => self.parse_float()?,
            TokenKind::String(_) => self.parse_string()?,
            TokenKind::Char(_) => self.parse_char()?,
            TokenKind::Bool(_) => self.parse_bool()?,
            _ => unreachable!(),
        };

        Some(value)
    }

    pub fn parse_value_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_value_expression()?;
        Some(Statement::Expression(expression))
    }

    pub fn parse_integer(&mut self) -> Option<Expression> {
        let (prefix, suffix, value) = consume!(
            self,
            TokenKind::Integer(prefix, suffix, value) => {
                (prefix, suffix, value)
            }
        );

        macro_rules! parse_int {
            ($repr: ident, $value_type: ident) => {{
                let value = $repr::from_str_radix(&value, prefix.into());
                Value::$value_type(value.unwrap())
            }};
        }

        macro_rules! parse_float {
            ($repr: ident, $value_type: ident) => {{
                let value = value.parse::<$repr>();
                Value::$value_type(value.unwrap())
            }};
        }

        let value = match suffix {
            NumberSuffix::I8 => parse_int!(i8, I8),
            NumberSuffix::I16 => parse_int!(i16, I16),
            NumberSuffix::I32 => parse_int!(i32, I32),
            NumberSuffix::I64 => parse_int!(i64, I64),

            NumberSuffix::U8 => parse_int!(u8, U8),
            NumberSuffix::U16 => parse_int!(u16, U16),
            NumberSuffix::U32 => parse_int!(u32, U32),
            NumberSuffix::U64 => parse_int!(u64, U64),

            NumberSuffix::F32 => parse_float!(f32, F32),
            NumberSuffix::F64 => parse_float!(f64, F64),

            NumberSuffix::None => {
                return Some(Expression::UnknownNumber(Some(prefix), suffix, value))
            }
        };

        Some(Expression::Value(value))
    }

    pub fn parse_float(&mut self) -> Option<Expression> {
        let (suffix, value) = consume!(
            self,
            TokenKind::Float(suffix, value) => {
                (suffix, value)
            }
        );

        macro_rules! parse_float {
            ($repr: ident, $value_type: ident) => {{
                let value = value.parse::<$repr>();
                Value::$value_type(value.unwrap())
            }};
        }

        let value = match suffix {
            NumberSuffix::F32 => parse_float!(f32, F32),
            NumberSuffix::F64 => parse_float!(f64, F64),
            NumberSuffix::None => return Some(Expression::UnknownNumber(None, suffix, value)),

            _ => {
                self.error(ParserError::InvalidFloatSuffix(InvalidFloatSuffix {
                    dbg_line: dbg_line!(),
                    suffix: suffix,
                    src: self.code.to_string(),
                    position: (&self.current_span).into(),
                }));
                return None;
            }
        };

        Some(Expression::Value(value))
    }

    pub fn parse_string(&mut self) -> Option<Expression> {
        let string = consume!(self, TokenKind::String(string) => string);
        Some(Expression::Value(Value::String(string)))
    }

    pub fn parse_char(&mut self) -> Option<Expression> {
        let character = consume!(self, TokenKind::Char(character) => character);
        Some(Expression::Value(Value::Char(character)))
    }

    pub fn parse_bool(&mut self) -> Option<Expression> {
        let boolean = consume!(self, TokenKind::Bool(boolean) => boolean);
        Some(Expression::Value(Value::Bool(boolean)))
    }

    pub fn parse_type(&mut self) -> Option<Type> {
        // Possible types:
        // - i8, i16, i32, i64
        // - u8, u16, u32, u64
        // - f32, f64
        // - bool
        // - char
        // - string
        // - type[]             (vec of type)
        // - type[size]         (array of type)
        // - &type              (ref to type)
        // - &mut type          (mut ref to type)
        // - (type, type, ..)   (tuple of types)
        let token = consume_from!(self, [TokenKind::Identifier(_), TokenKind::Ampersand]);

        let value_type = match token.0 {
            TokenKind::Identifier(identifier) => match identifier.as_str() {
                "i8" => Type::I8,
                "i16" => Type::I16,
                "i32" => Type::I32,
                "i64" => Type::I64,

                "u8" => Type::U8,
                "u16" => Type::U16,
                "u32" => Type::U32,
                "u64" => Type::U64,

                "f32" => Type::F32,
                "f64" => Type::F64,

                "bool" => Type::Bool,
                "char" => Type::Char,
                "string" => Type::String,

                ident => todo!("Type for identifier: {}", ident),
            },
            TokenKind::Ampersand => {
                match self.peek() {
                    Some(Token(TokenKind::Mut, _)) => {
                        consume!(self, TokenKind::Mut); // mut
                        let value_type = self.parse_type()?;
                        Type::MutableReference(Box::new(value_type))
                    }
                    _ => {
                        let value_type = self.parse_type()?;
                        Type::Reference(Box::new(value_type))
                    }
                }
            }
            _ => unreachable!(),
        };

        Some(value_type)
    }

    pub fn parse_variable_declaration(&mut self) -> Option<Statement> {
        // Possible statements:
        // let identifier = expression             (implicit type from expression)
        // let identifier: type = expression (explicit type, force expression to be of type)

        consume!(self, TokenKind::Let); // let

        let token = consume_from!(self, [TokenKind::Identifier(_), TokenKind::Mut]);
        let (mutable, identifier) = match token.0 {
            TokenKind::Identifier(identifier) => (false, identifier),
            TokenKind::Mut => (
                true,
                consume!(self, TokenKind::Identifier(identifier) => identifier),
            ),
            _ => unreachable!(),
        };

        let token = consume_from!(self, [TokenKind::Equals, TokenKind::Colon]);
        let (value_type, value) = match &token.0 {
            TokenKind::Equals => (
                Type::Unknown,
                self.parse_expression(&ExpressionContext::Default)?,
            ),
            TokenKind::Colon => {
                let value_type = self.parse_type()?;
                consume!(self, TokenKind::Equals);
                let value = self.parse_expression(&ExpressionContext::Default)?;
                (value_type, value)
            }
            _ => unreachable!(),
        };

        let interned_ident = self.interner.intern(&identifier);

        let varialble_declaration = Statement::VariableDeclaration(VariableDeclaration {
            identifier: interned_ident,
            mutable,
            value_type,
            value,
        });

        Some(varialble_declaration)
    }

    pub fn parse_function_expression(&mut self) -> Option<Expression> {
        // Possible statements:
        // fun identifier((ident: type,)*) { stmt[]; expr? }
        // fun identifier((ident: type,)*) -> type { stmt[]; expr? }

        consume!(self, TokenKind::Fun);

        let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
        let interned_ident = self.interner.intern(&identifier);

        consume!(self, TokenKind::LeftParen);

        // (ident: type,)*
        let parameters = if let TokenKind::RightParen = self.peek()?.0 {
            consume!(self, TokenKind::RightParen);
            None
        } else {
            let mut parameters = Vec::new();
            loop {
                // ident: type
                let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
                let interned_ident = self.interner.intern(&identifier);
                consume!(self, TokenKind::Colon);
                let value_type = self.parse_type()?;

                parameters.push((interned_ident, value_type));

                let token = consume_from!(self, [TokenKind::Comma, TokenKind::RightParen]);
                if let TokenKind::RightParen = token.0 {
                    break;
                }
            }
            Some(parameters.into_boxed_slice())
        };

        // -> type
        let return_type = match peek_from!(self, [TokenKind::RightArrow, TokenKind::LeftCurly]).0 {
            TokenKind::RightArrow => {
                self.consume_any();
                Some(self.parse_type()?)
            }
            TokenKind::LeftCurly => None,
            _ => unreachable!(),
        };

        // { stmt[]; expr? }
        let block = self.parse_block_expression(&ExpressionContext::Function)?;

        Some(Expression::Function(Function {
            identifier: interned_ident,
            parameters,
            return_type,
            block,
        }))
    }

    pub fn parse_function_statement(&mut self) -> Option<Statement> {
        Some(Statement::Expression(self.parse_function_expression()?))
    }

    pub fn parse_statement(&mut self, context: &ExpressionContext) -> Option<Statement> {
        let next = self.peek()?;

        let statement = match &next.0 {
            TokenKind::BlockComment(_) | TokenKind::LineComment(_) => {
                self.consume_any();
                return self.parse_statement(context);
            }

            TokenKind::Identifier(_) | value_pattern!() => {
                self.parse_expression_statement(context)?
            }
            TokenKind::Return => self.parse_return_statement(context)?,

            TokenKind::Let => self.parse_variable_declaration()?,

            TokenKind::If => self.parse_if_statement()?,
            TokenKind::Fun => self.parse_function_statement()?,
            TokenKind::LeftCurly => self.parse_block_statement()?,

            token => todo!("Token: {:?}", token),
        };

        Some(statement)
    }
}
