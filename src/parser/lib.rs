use std::collections::VecDeque;

use miette::{Result, SourceSpan};

use expression::{ExpressionContext, VariableAccess};
use lexer::{NumberPrefix, NumberSuffix, Token, TokenKind};
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
    expression::{
        BinaryOperation, Contained, Function, Return, StructInitialization,
        StructInitializationField, UnaryOperation,
    },
    statement::{StructDeclaration, StructDeclarationField},
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
            | TokenKind::LeftParen
            | TokenKind::LeftSquare
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
        // expr (op expr)*                                        (binary operation)
        // expr.ident                                             (property access)
        // if cond { ...stmt[]; expr? } else { ...stmt[]; expr? } (if expression)
        // { ...stmt[]; expr? }                                   (block)
        // identifier { (identifier: expr)+ }                     (enum initialization)

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

    pub fn parse_operand(&mut self, context: &ExpressionContext) -> Option<Expression> {
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
            TokenKind::Identifier(_) => self.parse_identifier_expression(context)?,
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

                // Call
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
        // Possible operands:
        // return operand
        // -operand

        let operator = match self.peek()?.0 {
            TokenKind::Not => {
                self.consume_any();
                Operator::Not
            }
            TokenKind::Subtract => {
                self.consume_any();
                Operator::Subtract
            }
            TokenKind::LeftParen => {
                // Either a contained expression or a tuple
                let operand = self.parse_operand(context)?;

                let operand = match operand {
                    // If the expression is a tuple with only one item
                    // We reinterpret it as contained expressionan
                    Expression::Value(Value::Tuple(values)) if values.len() == 1 => {
                        let value = values.into_vec().pop()?;
                        Expression::Value(value)
                    }
                    Expression::Tuple(expressions) if expressions.len() == 1 => {
                        let expression = expressions.into_vec().pop()?;
                        expression
                    }
                    // Actual tuple
                    _ => return Some(operand),
                };

                return Some(Expression::Contained(Contained {
                    expression: Box::new(operand),
                }));
            }
            _ => return self.parse_operand(context),
        };

        let operand = self.parse_operand_with_unary_operators(context)?;

        Some(Expression::UnaryOperation(UnaryOperation {
            operator,
            expression: Box::new(operand),
        }))
    }

    pub fn parse_return_statement(&mut self, context: &ExpressionContext) -> Option<Statement> {
        consume!(self, TokenKind::Return);
        let expression = self.parse_expression(context);
        Some(Statement::Expression(Expression::ExplicitReturn(Return {
            expression: Box::new(expression?),
        })))
    }

    pub fn parse_identifier_expression(
        &mut self,
        context: &ExpressionContext,
    ) -> Option<Expression> {
        // Possible expressions:
        // identifier                           (variable access)
        // identifier { (identifier: expr,)+ }  (struct initialization)

        let expression = match self.peek_next(1)?.0 {
            TokenKind::LeftCurly if matches!(context, ExpressionContext::Default) => {
                self.parse_struct_initialization()?
            }
            _ => self.parse_variable_access_expression()?,
        };

        Some(expression)
    }

    pub fn parse_variable_access_expression(&mut self) -> Option<Expression> {
        let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
        let interned_ident = self.interner.intern(&identifier);
        Some(Expression::VariableAccess(VariableAccess {
            identifier: interned_ident,
        }))
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
            TokenKind::LeftParen => self.parse_tuple()?,
            TokenKind::LeftSquare => self.parse_array_or_vec()?,
            _ => unreachable!(),
        };

        Some(value)
    }

    pub fn parse_value_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_value_expression()?;
        Some(Statement::Expression(expression))
    }

    pub fn parse_tuple(&mut self) -> Option<Expression> {
        // Possible expressions:
        // ((expr,)*)
        // ((value,)*)
        // (((expr|value),)*)

        consume!(self, TokenKind::LeftParen);

        let mut values = Vec::new();

        loop {
            values.push(self.parse_expression(&ExpressionContext::NoSemicolon)?);

            let token = consume_from!(self, [TokenKind::Comma, TokenKind::RightParen]);
            if let TokenKind::RightParen = token.0 {
                break;
            }
        }

        // Check if tuple can be converted directly to VM::Value
        // If so, do it
        let full_of_values = values
            .iter()
            .all(|expr| matches!(expr, Expression::Value(_)));
        if full_of_values {
            let values = values
                .into_iter()
                .map(|expr| match expr {
                    Expression::Value(value) => value,
                    _ => unreachable!(),
                })
                .collect::<Box<_>>();
            return Some(Expression::Value(Value::Tuple(values)));
        }

        Some(Expression::Tuple(values.into_boxed_slice()))
    }

    pub fn parse_array_or_vec(&mut self) -> Option<Expression> {
        if matches!(self.peek_next(1), Some(Token(TokenKind::Pipe, _))) {
            self.parse_vec()
        } else {
            self.parse_array()
        }
    }

    pub fn parse_array(&mut self) -> Option<Expression> {
        // Possible expressions:
        // [(expr,)*]
        // [(value,)*]
        // [(expr|value,)*]

        consume!(self, TokenKind::LeftSquare);

        let mut values = Vec::new();

        loop {
            values.push(self.parse_expression(&ExpressionContext::NoSemicolon)?);

            let token = consume_from!(self, [TokenKind::Comma, TokenKind::RightSquare]);
            if let TokenKind::RightSquare = token.0 {
                break;
            }
        }

        // Check if array can be converted directly to VM::Value
        // If so, do it
        let full_of_values = values
            .iter()
            .all(|expr| matches!(expr, Expression::Value(_)));
        if full_of_values {
            let values = values
                .into_iter()
                .map(|expr| match expr {
                    Expression::Value(value) => value,
                    _ => unreachable!(),
                })
                .collect::<Box<_>>();
            return Some(Expression::Value(Value::Array(values)));
        }

        Some(Expression::Array(values.into_boxed_slice()))
    }

    pub fn parse_vec(&mut self) -> Option<Expression> {
        // Possible expressions:
        // [|(expr,)*|]
        // [|(value,)*|]
        // [|(expr|value,)*|]

        consume!(self, TokenKind::LeftSquare);
        consume!(self, TokenKind::Pipe);

        let mut values = Vec::new();

        loop {
            values.push(self.parse_expression(&ExpressionContext::NoSemicolon)?);

            let token = consume_from!(self, [TokenKind::Comma, TokenKind::Pipe]);
            if let TokenKind::Pipe = token.0 {
                break;
            }
        }

        consume!(self, TokenKind::RightSquare);

        // Check if vec can be converted directly to VM::Value
        // If so, do it
        let full_of_values = values
            .iter()
            .all(|expr| matches!(expr, Expression::Value(_)));
        if full_of_values {
            let values = values
                .into_iter()
                .map(|expr| match expr {
                    Expression::Value(value) => value,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            return Some(Expression::Value(Value::Vector(values)));
        }

        Some(Expression::Vector(values))
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
        let token = peek_from!(
            self,
            [
                TokenKind::Identifier(_),
                TokenKind::Ampersand,
                TokenKind::LeftParen
            ]
        );

        let mut value_type = match token.0 {
            TokenKind::Identifier(_) => self.parse_identifier_type()?,
            TokenKind::Ampersand => self.parse_reference_type()?,
            TokenKind::LeftParen => self.parse_tuple_type()?,
            _ => unreachable!(),
        };

        // Array or Vector
        if let TokenKind::LeftSquare = self.peek()?.0 {
            consume!(self, TokenKind::LeftSquare);
            let token = consume_from!(self, [TokenKind::RightSquare, TokenKind::Integer(..)]);

            if let TokenKind::Integer(prefix, suffix, number) = token.0 {
                if !matches!(prefix, NumberPrefix::None) || !matches!(suffix, NumberSuffix::None) {
                    // TODO: Number in array can't have prefixes and/or suffixes
                    return None;
                }

                let Ok(size) = number.parse::<u32>() else {
                    // TODO: Invalid array size error
                    return None;
                };

                consume!(self, TokenKind::RightSquare);

                value_type = Type::Array(Box::new(value_type), size);
            } else {
                value_type = Type::Vector(Box::new(value_type));
            }
        }

        Some(value_type)
    }

    pub fn parse_identifier_type(&mut self) -> Option<Type> {
        let identifier = consume!(
            self,
            TokenKind::Identifier(identifier) => identifier
        );

        let value_type = match identifier.as_str() {
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

            identifier => {
                let interned_ident = self.interner.intern(&identifier);
                Type::Custom(interned_ident)
            }
        };

        Some(value_type)
    }

    pub fn parse_reference_type(&mut self) -> Option<Type> {
        let value_type = if let TokenKind::Mut = self.peek()?.0 {
            consume!(self, TokenKind::Mut); // mut
            let value_type = self.parse_type()?;
            Type::MutableReference(Box::new(value_type))
        } else {
            let value_type = self.parse_type()?;
            Type::Reference(Box::new(value_type))
        };

        Some(value_type)
    }

    pub fn parse_tuple_type(&mut self) -> Option<Type> {
        // Possible types:
        // ((type,)*)

        consume!(self, TokenKind::LeftParen);

        let mut types = Vec::new();

        loop {
            types.push(self.parse_type()?);

            let token = consume_from!(self, [TokenKind::Comma, TokenKind::RightParen]);
            if let TokenKind::RightParen = token.0 {
                break;
            }
        }

        Some(Type::Tuple(types.into_boxed_slice()))
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

    pub fn parse_struct_initialization(&mut self) -> Option<Expression> {
        // Possible expressions:
        // identifier { (ident: expr,)+ }

        let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
        let interned_ident = self.interner.intern(&identifier);

        consume!(self, TokenKind::LeftCurly);

        let mut fields = Vec::new();
        loop {
            // ident: expr
            let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
            let interned_ident = self.interner.intern(&identifier);
            consume!(self, TokenKind::Colon);

            let expression = self.parse_expression(&ExpressionContext::NoSemicolon)?;

            fields.push(StructInitializationField {
                identifier: interned_ident,
                expression,
            });

            let token = consume_from!(self, [TokenKind::Comma, TokenKind::RightCurly]);
            if let TokenKind::RightCurly = token.0 {
                break;
            }
        }

        Some(Expression::StructInitialization(StructInitialization {
            identifier: interned_ident,
            fields: fields.into_boxed_slice(),
        }))
    }

    pub fn parse_struct_declaration(&mut self) -> Option<Statement> {
        // Possible statements:
        // struct identifier { (ident: type,)+ }

        consume!(self, TokenKind::Struct);

        let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
        let interned_ident = self.interner.intern(&identifier);

        consume!(self, TokenKind::LeftCurly);

        let mut fields = Vec::new();
        loop {
            // ident: type
            let identifier = consume!(self, TokenKind::Identifier(identifier) => identifier);
            let interned_ident = self.interner.intern(&identifier);
            consume!(self, TokenKind::Colon);
            let value_type = self.parse_type()?;

            fields.push(StructDeclarationField {
                identifier: interned_ident,
                value_type,
            });

            let token = consume_from!(self, [TokenKind::Comma, TokenKind::RightCurly]);
            if let TokenKind::RightCurly = token.0 {
                break;
            }
        }

        Some(Statement::StructDeclaration(StructDeclaration {
            identifier: interned_ident,
            fields: fields.into_boxed_slice(),
        }))
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

            TokenKind::Struct => self.parse_struct_declaration()?,
            TokenKind::Enum => todo!("Enum"),

            token => todo!("Token: {:?}", token),
        };

        Some(statement)
    }
}
