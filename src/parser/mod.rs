mod ast;
mod errors;
mod expressions;
mod patterns;
mod statements;

use std::collections::VecDeque;

pub use self::{
    expressions::{
        BlockExpression, Expression, ExpressionSpan, FunctionCallExpression, IfExpression,
        LogicalComparison, LogicalComparisonExpression, MathOperation, MathOperationExpression,
        OrderComparison, OrderComparisonExpression,
    },
    patterns::Pattern,
    statements::{EnumVariant, FunctionArg, Statement, StatementBody, StructField},
};

use crate::{
    lexer::{Token, TokenSpan},
    shared::{
        name_bihasher::{NameBihasher, NameHash},
        types::Type,
    },
};

pub use ast::AST;

use miette::{bail, Result};
use rustc_hash::FxHasher;

const TOKEN_TYPE: &Token = &Token::Type("");
const TOKEN_IDENTIFIER: &Token = &Token::Identifier("");
const TOKEN_RETURN_TYPE: &Token = &Token::ReturnType(Type::Unknown);
const TOKEN_DUBIOUS_RETURN_TYPE: &Token = &Token::DubiousReturnType(Type::Unknown);

macro_rules! at_dbg_line {
    () => {
        format!("{}:{}", file!(), line!())
    };
}

macro_rules! pop_expression_stack {
    ($self:ident, $expression_stack:ident) => {
        if let Some(item) = $expression_stack.pop() {
            item
        } else {
            bail!(errors::EmptyExpressionStack {
                dbg_line: at_dbg_line!(),
                src: $self.code.to_string(),
                position: {
                    let span = $self.current_span()?;
                    (span.from..span.to).into()
                },
            })
        }
    };
}

pub struct Parser<'a> {
    cursor: usize,
    code: &'a str,
    input: Vec<TokenSpan<'a>>,

    bihasher: NameBihasher<FxHasher>,
    type_suggestions: Vec<Type>,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str, tokens: Vec<TokenSpan<'a>>) -> Self {
        Parser {
            cursor: 0,
            code,
            input: tokens,

            bihasher: NameBihasher::default(),
            type_suggestions: Vec::new(),
        }
    }

    pub fn nth_span(&self, n: usize) -> Result<&TokenSpan<'a>> {
        if let Some(span) = self.input.get(n) {
            Ok(span)
        } else {
            bail!(errors::UnexpectedEndOfFile {
                dbg_line: at_dbg_line!(),
                src: self.code.to_string(),
                position: {
                    if self.cursor >= self.input.len() {
                        let span = &self.input[self.input.len() - 1];
                        (span.from..span.to).into()
                    } else {
                        let span = &self.input[self.cursor];
                        (span.from..span.to).into()
                    }
                },
            })
        }
    }

    pub fn current_span(&self) -> Result<&TokenSpan<'a>> {
        self.nth_span(self.cursor)
    }

    pub fn peek_next_n_span(&self, n: usize) -> Option<&TokenSpan<'a>> {
        self.input.get(self.cursor + n)
    }

    pub fn current(&self) -> Result<&Token<'a>> {
        Ok(&self.current_span()?.token)
    }

    pub fn peek_next_n(&self, n: usize) -> Result<&Token<'a>> {
        if self.cursor + n < self.input.len() {
            Ok(&self.input[self.cursor + n].token)
        } else {
            bail!(errors::UnexpectedEndOfFile {
                dbg_line: at_dbg_line!(),
                src: self.code.to_string(),
                position: {
                    let span = self.current_span()?;
                    (span.from..span.to).into()
                },
            })
        }
    }

    pub fn expect_either_current(&self, tokens: &[&Token]) -> Result<()> {
        self.expect_either_next_n(0, tokens)
    }

    pub fn expect_either_next_n(&self, n: usize, tokens: &[&Token]) -> Result<()> {
        let actual = self.peek_next_n_span(n);
        let Some(actual) = actual else {
            bail!(errors::UnexpectedEndOfFile {
                dbg_line: at_dbg_line!(),
                src: self.code.to_string(),
                position: {
                    if self.cursor >= self.input.len() {
                        let span = &self.input[self.input.len() - 1];
                        (span.from..span.to).into()
                    } else {
                        let span = self.current_span()?;
                        (span.from..span.to).into()
                    }
                },
            })
        };

        for token in tokens {
            if actual.token.eq_type(token) {
                return Ok(());
            }
        }

        bail!(errors::UnexpectedToken {
            dbg_line: at_dbg_line!(),
            actual: format!("{:?}", actual),
            expected: format!("any of {:?}", tokens),
            src: self.code.to_string(),
            position: (actual.from..actual.to).into(),
        });
    }

    pub fn expect_current(&self, token: &Token) -> Result<()> {
        self.expect_next_n(0, token)
    }

    pub fn expect_next_n<'t>(&'t self, n: usize, token: &Token) -> Result<()> {
        let peeked = self.peek_next_n(n)?;
        if peeked.eq_type(token) {
            Ok(())
        } else {
            bail!(errors::UnexpectedToken {
                dbg_line: at_dbg_line!(),
                actual: format!("{:?}", peeked),
                expected: format!("{:?}", token),
                src: self.code.to_string(),
                position: {
                    let span = self.current_span()?;
                    (span.from..span.to).into()
                },
            });
        }
    }

    pub fn parse(mut self) -> Result<AST<'a>> {
        let mut body = VecDeque::new();

        while self.cursor < self.input.len() {
            self.skip_comments_and_whitespace()?;
            if self.cursor >= self.input.len() {
                break;
            }

            body.push_back(self.parse_statement()?);
        }

        Ok(AST {
            body,
            bihasher: self.bihasher,
        })
    }

    pub fn skip_comments_and_whitespace(&mut self) -> Result<()> {
        while self.cursor < self.input.len() {
            match self.current()? {
                Token::LineComment(_) | Token::Semicolon => {
                    self.cursor += 1;
                }
                _ => break,
            }
        }

        Ok(())
    }

    pub fn parse_statement(&mut self) -> Result<Statement<'a>> {
        match self.current()? {
            Token::VariableDeclaration => self.parse_variable_declaration(),
            Token::Function => self.parse_function(),
            Token::Enum => self.parse_enum(),
            Token::Struct => self.parse_struct(),
            Token::LeftCurly => self.parse_block_statement(),
            Token::If => self.parse_if_statement(),
            Token::For => self.parse_for_loop(),
            Token::While => self.parse_while_loop(),
            Token::Loop => self.parse_loop(),

            Token::Identifier(_)
            | Token::Integer32(_)
            | Token::Integer64(_)
            | Token::UnknownInteger(_)
            | Token::Float32(_)
            | Token::Float64(_)
            | Token::UnknownFloat(_)
            | Token::String(_)
            | Token::Bool(_)
            | Token::LeftBracket
            | Token::LeftParen => self.parse_expression_statement(),

            Token::Continue => {
                self.cursor += 1;
                Ok(Statement::Continue)
            }
            Token::Break => {
                self.cursor += 1;
                Ok(Statement::Break)
            }

            Token::Return => {
                self.cursor += 1;
                Ok(Statement::Return(
                    self.parse_expression(&[&Token::Semicolon])?,
                ))
            }
            Token::Bail => {
                self.cursor += 1;
                Ok(Statement::Bail(
                    self.parse_expression(&[&Token::Semicolon])?,
                ))
            }

            _ => bail!(errors::UnexpectedToken {
                dbg_line: at_dbg_line!(),
                actual: format!("{:?}", self.current()),
                expected: format!("statement token (maybe this one is unimplemented?)"),
                src: self.code.to_string(),
                position: {
                    let span = self.current_span()?;
                    (span.from..span.to).into()
                },
            }),
        }
    }

    pub fn check_expression_stack_for_missing_end(
        &self,
        expression_stack: &Vec<ExpressionSpan<'_>>,
        expected_end: &[&Token],
    ) -> Result<()> {
        if expression_stack.len() <= 1 {
            return Ok(());
        }

        // Check whether we just missed a semicolon at some point
        for ExpressionSpan { to, .. } in expression_stack {
            let span = &self.input[*to];
            let token = &span.token;
            if !expected_end.contains(&token) {
                let previous_span = &self.input[to - 1];
                bail!(errors::MissingEnd {
                    dbg_line: at_dbg_line!(),
                    end: format!("{:?}", expected_end),
                    src: self.code.to_string(),
                    position: (previous_span.to..previous_span.to).into(),
                })
            }
        }

        // If expression stack is nonempty for other reason, display the problem
        let start_span = &expression_stack[0];
        let current_span = self.current_span()?;

        bail!(errors::NonemptyExpressionStack {
            dbg_line: at_dbg_line!(),
            src: self.code.to_string(),
            position: (start_span.from..current_span.to).into(),
        });
    }

    pub fn parse_expression(&mut self, expected_end: &[&Token]) -> Result<Expression<'a>> {
        let mut expression_stack: Vec<ExpressionSpan<'a>> = Vec::new();

        for iteration_cursor in self.cursor..self.input.len() {
            let current = match self.current()? {
                ref token if expected_end.contains(token) => break,

                Token::Float32(_)
                | Token::Float64(_)
                | Token::Integer32(_)
                | Token::Integer64(_)
                | Token::Bool(_)
                | Token::String(_)
                | Token::LeftBracket => {
                    let value = ExpressionSpan::new(
                        self.parse_value_expression()?,
                        iteration_cursor,
                        self.cursor,
                    );
                    self.cursor += 1;
                    value
                }

                Token::Access => {
                    let item = pop_expression_stack!(self, expression_stack).expression;

                    self.cursor += 1;
                    let property = self.parse_name_identifier()?;

                    ExpressionSpan::new(
                        Expression::AccessProperty(item.into(), property),
                        iteration_cursor,
                        self.cursor,
                    )
                }

                Token::LeftCurly => ExpressionSpan::new(
                    self.parse_block_expression()?,
                    iteration_cursor,
                    self.cursor,
                ),

                Token::If => {
                    ExpressionSpan::new(self.parse_if_expression()?, iteration_cursor, self.cursor)
                }

                Token::LeftParen => ExpressionSpan::new(
                    self.parse_possible_left_paren_expressions(&mut expression_stack)?,
                    iteration_cursor,
                    self.cursor,
                ),

                Token::Identifier(identifier) => {
                    let hashed_identifier = self.bihasher.hash(*identifier);
                    let expression = Expression::VariableReference(hashed_identifier);
                    self.cursor += 1;
                    ExpressionSpan::new(expression, iteration_cursor, self.cursor)
                }

                Token::Assign => {
                    let left_side_span = pop_expression_stack!(self, expression_stack);
                    let left_side = left_side_span.expression;

                    self.cursor += 1;
                    let right_side = self.parse_expression(expected_end)?;

                    if let Expression::VariableReference(identifier) = left_side {
                        ExpressionSpan::new(
                            Expression::VariableAssignment(identifier, right_side.into()),
                            iteration_cursor,
                            self.cursor,
                        )
                    } else {
                        bail!(errors::InvalidLeftHandSideAssignment {
                            dbg_line: at_dbg_line!(),
                            src: self.code.to_string(),
                            left_side: format!("{:?}", left_side),
                            position: { (left_side_span.from..left_side_span.to).into() },
                        })
                    }
                }
                Token::Add | Token::Modulo | Token::Subtract | Token::Multiply | Token::Divide => {
                    ExpressionSpan::new(
                        self.parse_math_operation_expression(&mut expression_stack, expected_end)?,
                        iteration_cursor,
                        self.cursor,
                    )
                }

                // TODO: Split long stuff to their own functions
                Token::AddAssign
                | Token::ModuloAssign
                | Token::SubtractAssign
                | Token::MutliplyAssign
                | Token::DivideAssign => ExpressionSpan::new(
                    self.parse_math_operation_assignment_expression(
                        &mut expression_stack,
                        expected_end,
                    )?,
                    iteration_cursor,
                    self.cursor,
                ),

                token @ (Token::And | Token::Or | Token::Nor | Token::Nand) => {
                    let comparison: LogicalComparison = token.try_into()?;

                    let left_side = pop_expression_stack!(self, expression_stack).expression;

                    self.cursor += 1;
                    let right_side = self.parse_expression(expected_end)?;

                    ExpressionSpan::new(
                        Expression::LogicalComparison(LogicalComparisonExpression(
                            left_side.into(),
                            right_side.into(),
                            comparison,
                        )),
                        iteration_cursor,
                        self.cursor,
                    )
                }

                Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::GreaterThan
                | Token::LessThanOrEqual
                | Token::GreaterThanOrEqual => ExpressionSpan::new(
                    self.parse_order_comparison_expression(&mut expression_stack, expected_end)?,
                    iteration_cursor,
                    self.cursor,
                ),

                Token::ExclusiveRange => {
                    let left_side = pop_expression_stack!(self, expression_stack).expression;

                    self.cursor += 1;
                    let right_side = self.parse_expression(expected_end)?;

                    ExpressionSpan::new(
                        Expression::ExclusiveRange(left_side.into(), right_side.into()),
                        iteration_cursor,
                        self.cursor,
                    )
                }

                Token::InclusiveRange => {
                    let left_side = pop_expression_stack!(self, expression_stack).expression;

                    self.cursor += 1;
                    let right_side = self.parse_expression(expected_end)?;

                    ExpressionSpan::new(
                        Expression::InclusiveRange(left_side.into(), right_side.into()),
                        iteration_cursor,
                        self.cursor,
                    )
                }

                Token::Bang => {
                    let expression = if expression_stack.is_empty() {
                        self.cursor += 1;
                        let right_side = self.parse_expression(expected_end)?;
                        Expression::Negate(right_side.into())
                    } else {
                        let left_side = pop_expression_stack!(self, expression_stack).expression;
                        Expression::QuickPanic(left_side.into())
                    };

                    ExpressionSpan::new(expression, iteration_cursor, self.cursor)
                }

                Token::QuestionMark => {
                    let left_side = pop_expression_stack!(self, expression_stack).expression;
                    self.cursor += 1;
                    ExpressionSpan::new(
                        Expression::QuickBail(left_side.into()),
                        iteration_cursor,
                        self.cursor,
                    )
                }

                token => match token {
                    Token::UnknownFloat(_) => ExpressionSpan::new(
                        self.parse_unknown_float_expression()?,
                        iteration_cursor,
                        self.cursor,
                    ),
                    Token::UnknownInteger(_) => ExpressionSpan::new(
                        self.parse_unknown_integer_expression()?,
                        iteration_cursor,
                        self.cursor,
                    ),
                    _ => {
                        self.check_expression_stack_for_missing_end(
                            &expression_stack,
                            expected_end,
                        )?;

                        panic!("");
                        bail!(errors::UnexpectedToken {
                            dbg_line: at_dbg_line!(),
                            actual: format!("{:?}", token),
                            expected: "expression token".into(),
                            src: self.code.to_string(),
                            position: {
                                let span = self.current_span()?;
                                (span.from..span.to).into()
                            },
                        })
                    }
                },
            };

            println!("current: {current:?}");
            // just create a stack, take when it needs to consume an expression
            // and return just pops last element from the stack
            expression_stack.push(current);
        }

        self.expect_either_current(expected_end)?;
        self.check_expression_stack_for_missing_end(&expression_stack, expected_end)?;

        // FIXME: Math order

        if let Some(expression) = expression_stack.pop() {
            Ok(expression.expression)
        } else {
            bail!(self
                .current_span()?
                .report(self.code, "Expected expression, got nothing instead"))
        }
    }

    pub fn parse_value_expression(&mut self) -> Result<Expression<'a>> {
        let expression = match self.current()? {
            Token::Integer32(int) => Expression::Integer32(*int),
            Token::Integer64(int) => Expression::Integer64(*int),
            Token::Float32(float) => Expression::Float32(*float),
            Token::Float64(float) => Expression::Float64(*float),
            Token::Bool(bool) => Expression::Boolean(*bool),
            Token::String(string) => Expression::String(*string),

            Token::LeftParen => {
                // parse tuple
                self.cursor += 1;

                let mut items = Vec::with_capacity(2);
                loop {
                    if matches!(self.current()?, Token::RightParen) {
                        break;
                    }

                    items.push(self.parse_expression(&[&Token::Comma, &Token::RightParen])?);

                    self.expect_either_current(&[&Token::Comma, &Token::RightParen])?;
                    if matches!(self.current()?, Token::Comma) {
                        self.cursor += 1;
                    }
                }

                if items.len() == 1 {
                    Expression::ContainedExpression(items.pop().unwrap().into())
                } else {
                    Expression::Tuple(items.into())
                }
            }

            Token::LeftBracket => {
                // parse slice/array
                self.cursor += 1;

                let is_slice = if matches!(self.current()?, Token::Pipe) {
                    self.cursor += 1;
                    true
                } else {
                    false
                };

                let (total_end, expected_end) = if is_slice {
                    (Token::Pipe, &[&Token::Comma, &Token::Pipe])
                } else {
                    (Token::RightBracket, &[&Token::Comma, &Token::RightBracket])
                };

                let mut values = Vec::with_capacity(2);
                loop {
                    if self.current()?.eq_type(&total_end) {
                        break;
                    }

                    values.push(self.parse_expression(expected_end)?);

                    self.expect_either_current(expected_end)?;
                    if matches!(self.current()?, Token::Comma) {
                        self.cursor += 1;
                    }
                }

                if is_slice {
                    if !matches!(self.current()?, Token::Pipe) {
                        bail!(errors::MissingEnd {
                            dbg_line: at_dbg_line!(),
                            end: format!("{:?}", expected_end),
                            src: self.code.to_string(),
                            position: {
                                let span = self.current_span()?;
                                (span.from..span.to).into()
                            },
                        })
                    }
                    self.cursor += 1;

                    Expression::Slice(values.into())
                } else {
                    Expression::Array(values.into())
                }
            }

            token => bail!(errors::UnexpectedToken {
                dbg_line: at_dbg_line!(),
                actual: format!("{:?}", token),
                expected: "float, integer, bool, string, left paren, left bracket".into(),
                src: self.code.to_string(),
                position: {
                    let span = self.current_span()?;
                    (span.from..span.to).into()
                },
            }),
        };

        self.cursor += 1;
        Ok(expression)
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement<'a>> {
        Ok(Statement::Expression(
            self.parse_expression(&[&Token::Semicolon])?,
        ))
    }

    pub fn parse_name_identifier(&mut self) -> Result<NameHash> {
        self.expect_current(TOKEN_IDENTIFIER)?;
        let Token::Identifier(name) = *self.current()? else {
            unreachable!()
        };
        self.cursor += 1;

        let hashed_name = self.bihasher.hash(name);
        Ok(hashed_name)
    }

    pub fn parse_type_identifier(&mut self) -> Result<Type> {
        self.expect_current(TOKEN_IDENTIFIER)?;
        let Token::Identifier(value_type) = *self.current()? else {
            unreachable!()
        };
        self.cursor += 1;
        Ok(value_type.try_into()?)
    }

    pub fn parse_type(&mut self) -> Result<Type> {
        self.expect_current(TOKEN_TYPE)?;

        let Token::Type(value_type) = *self.current()? else {
            unreachable!()
        };
        self.cursor += 1;
        Ok(value_type.try_into()?)
    }

    pub fn parse_type_specifier(&mut self) -> Result<Type> {
        self.expect_current(&Token::Colon)?;
        self.cursor += 1;

        self.parse_type()
    }

    pub fn parse_body(&mut self) -> Result<StatementBody<'a>> {
        let mut body = Vec::new();

        self.expect_current(&Token::LeftCurly)?;
        self.cursor += 1;

        loop {
            self.skip_comments_and_whitespace()?;
            if matches!(self.current()?, Token::RightCurly) {
                self.cursor += 1;
                break;
            }

            body.push(self.parse_statement()?);
        }

        Ok(body.into())
    }

    pub fn parse_function_arguments(&mut self) -> Result<Vec<FunctionArg<'a>>> {
        let mut args = Vec::new();

        self.expect_current(&Token::LeftParen)?;
        self.cursor += 1;

        loop {
            if matches!(self.current()?, Token::RightParen) {
                break;
            }

            let name = self.parse_name_identifier()?;

            let value_type = self.parse_type_specifier()?;

            args.push((name, value_type));

            self.expect_either_current(&[&Token::Comma, &Token::RightParen])?;
            if matches!(self.current()?, Token::Comma) {
                self.cursor += 1;
            }
        }

        self.cursor += 1;
        Ok(args.into())
    }

    pub fn parse_function_return_type(&mut self) -> Result<Type> {
        self.expect_either_current(&[TOKEN_RETURN_TYPE, TOKEN_DUBIOUS_RETURN_TYPE])?;

        let return_type = match self.current()? {
            Token::ReturnType(return_type) | Token::DubiousReturnType(return_type) => {
                return_type.clone()
            }
            _ => unreachable!(),
        };

        self.cursor += 1;
        Ok(return_type)
    }

    pub fn parse_function(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::Function)?;
        self.cursor += 1;

        let name = self.parse_name_identifier()?;
        let args = self.parse_function_arguments()?;
        let return_type = self.parse_function_return_type()?;
        // TODO: This is possibly error prone
        // This should be fixed when we have a proper static type checker
        self.type_suggestions.push(return_type);
        let body = self.parse_body()?;
        let return_type = self.type_suggestions.pop().unwrap();

        Ok(Statement::Function(name, args.into(), return_type, body))
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::VariableDeclaration)?;
        self.cursor += 1;

        let is_mutable = if matches!(self.current()?, Token::Mutable) {
            self.cursor += 1;
            true
        } else {
            false
        };

        let pattern = self.parse_pattern()?;

        let (value, value_type) = if matches!(self.current()?, &Token::Colon) {
            // Explicit typing
            self.expect_current(&Token::Colon)?;
            self.cursor += 1;

            self.expect_current(&TOKEN_TYPE)?;
            let Token::Type(value_type) = *self.current()? else {
                unreachable!()
            };
            let value_type: Type = value_type.try_into()?;
            self.cursor += 1;

            self.expect_current(&Token::Assign)?;
            self.cursor += 1;

            self.type_suggestions.push(value_type);
            let value = self.parse_expression(&[&Token::Semicolon])?;
            let value_type = self.type_suggestions.pop().unwrap();

            (value, value_type)
        } else {
            // Implicit typing
            self.expect_current(&Token::Assign)?;
            self.cursor += 1;

            println!("{:?}", self.current()?);
            let value = self.parse_expression(&[&Token::Semicolon])?;

            (value, Type::Unknown)
        };

        Ok(Statement::VariableDeclaration(
            pattern, is_mutable, value_type, value,
        ))
    }

    pub fn parse_enum(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::Enum)?;
        self.cursor += 1;

        let name = self.parse_name_identifier()?;

        self.expect_current(&Token::LeftCurly)?;
        self.cursor += 1;

        let mut variants: Vec<EnumVariant<'a>> = Vec::new();
        let mut current_discriminant: i32 = 0;
        loop {
            match *self.current()? {
                Token::RightCurly => break,

                Token::Identifier(variant) => {
                    self.cursor += 1;

                    let (discriminant, field_type) = if matches!(self.current()?, Token::Assign) {
                        // explicit discriminant
                        self.cursor += 1;
                        let expression = self.parse_expression(&[&Token::Comma])?;
                        (expression, None)
                    } else if matches!(self.current()?, Token::LeftParen) {
                        // tagged enum (with fields)
                        self.cursor += 1;

                        let field_type = self.parse_type_identifier()?;

                        self.expect_current(&Token::RightParen)?;
                        self.cursor += 1;

                        current_discriminant += 1;
                        (
                            Expression::Integer32(current_discriminant),
                            Some(field_type),
                        )
                    } else {
                        // implicit discriminant
                        current_discriminant += 1;
                        (Expression::Integer32(current_discriminant), None)
                    };

                    self.expect_current(&Token::Comma)?;

                    variants.push((variant, discriminant, field_type))
                }

                ref token => bail!(errors::UnexpectedToken {
                    dbg_line: at_dbg_line!(),
                    actual: format!("{:?}", token),
                    expected: "identifier, right curly, assign, left paren".into(),
                    src: self.code.to_string(),
                    position: {
                        let span = self.current_span()?;
                        (span.from..span.to).into()
                    },
                }),
            };

            self.cursor += 1;
        }

        self.expect_current(&Token::RightCurly)?;
        self.cursor += 1;

        Ok(Statement::EnumDeclaration(name, variants))
    }

    pub fn parse_struct(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::Struct)?;
        self.cursor += 1;

        let name = self.parse_name_identifier()?;

        self.expect_current(&Token::LeftCurly)?;
        self.cursor += 1;

        let mut fields: Vec<StructField<'a>> = Vec::new();
        loop {
            match *self.current()? {
                Token::RightCurly => break,

                Token::Identifier(field_key) => {
                    self.cursor += 1;

                    self.expect_current(&Token::Colon)?;
                    self.cursor += 1;

                    let field_type = self.parse_type()?;

                    fields.push((field_key, field_type))
                }

                ref token => bail!(self.current_span()?.report(
                    self.code,
                    format!("<{}> Unexpected token: {token:?}", line!())
                )),
            };

            self.cursor += 1;
        }

        self.expect_current(&Token::RightCurly)?;
        self.cursor += 1;

        Ok(Statement::StructDeclaration(name, fields))
    }

    pub fn parse_block_statement(&mut self) -> Result<Statement<'a>> {
        Ok(Statement::Expression(self.parse_block_expression()?))
    }

    pub fn parse_block_expression_raw(&mut self) -> Result<BlockExpression<'a>> {
        Ok(BlockExpression(self.parse_body()?))
    }

    pub fn parse_block_expression(&mut self) -> Result<Expression<'a>> {
        Ok(Expression::Block(self.parse_block_expression_raw()?))
    }

    pub fn parse_if_expression(&mut self) -> Result<Expression<'a>> {
        self.expect_current(&Token::If)?;
        self.cursor += 1;

        let condition = self.parse_expression(&[&Token::LeftCurly])?;

        let body = self.parse_block_expression_raw()?;

        let else_body = if matches!(self.current(), Ok(Token::Else)) {
            self.cursor += 1;

            let else_expression = if matches!(self.current()?, Token::If) {
                self.parse_if_expression()
            } else {
                self.parse_block_expression()
            }?;

            Some(else_expression.into())
        } else {
            None
        };

        Ok(Expression::If(IfExpression(
            condition.into(),
            body.into(),
            else_body,
        )))
    }

    pub fn parse_if_statement(&mut self) -> Result<Statement<'a>> {
        Ok(Statement::Expression(self.parse_if_expression()?))
    }

    pub fn parse_pattern(&mut self) -> Result<Pattern> {
        match *self.current()? {
            Token::Mutable => {
                self.cursor += 1;

                let start = self.cursor;
                let pattern = self.parse_pattern()?;

                if let Pattern::Mutable(_) = pattern {
                    bail!(errors::InvalidMutablePattern {
                        dbg_line: at_dbg_line!(),
                        src: self.code.to_string(),
                        position: self.nth_span(start)?.into(),
                    })
                }

                Ok(Pattern::Mutable(Box::new(pattern)))
            }

            Token::Identifier(identifier) => {
                self.cursor += 1;
                let pattern = if identifier == "_" {
                    Pattern::Wildcard
                } else {
                    let hashed_identifier = self.bihasher.hash(identifier);
                    Pattern::Identifier(hashed_identifier)
                };
                Ok(pattern)
            }

            Token::ExclusiveRange => {
                self.cursor += 1;
                Ok(Pattern::Rest)
            }

            Token::LeftParen => {
                self.cursor += 1;

                let mut patterns = Vec::new();
                loop {
                    if matches!(self.current()?, Token::RightParen) {
                        break;
                    }

                    patterns.push(self.parse_pattern()?);

                    self.expect_either_current(&[&Token::Comma, &Token::RightParen])?;
                    if matches!(self.current()?, Token::Comma) {
                        self.cursor += 1;
                    }
                }

                self.cursor += 1;
                Ok(Pattern::Tuple(patterns))
            }

            Token::LeftBracket => {
                self.cursor += 1;

                let mut patterns = Vec::new();
                loop {
                    if matches!(self.current()?, Token::RightBracket) {
                        break;
                    }

                    patterns.push(self.parse_pattern()?);

                    self.expect_either_current(&[&Token::Comma, &Token::RightBracket])?;
                    if matches!(self.current()?, Token::Comma) {
                        self.cursor += 1;
                    }
                }

                self.cursor += 1;
                Ok(Pattern::Array(patterns))
            }

            ref token => {
                bail!(errors::UnexpectedToken {
                    dbg_line: at_dbg_line!(),
                    actual: format!("{:?}", token),
                    expected: "identifier, exclusive range, left paren, left bracket".into(),
                    src: self.code.to_string(),
                    position: {
                        let span = self.current_span()?;
                        (span.from..span.to).into()
                    },
                })
            }
        }
    }

    pub fn parse_for_loop(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::For)?;
        self.cursor += 1;

        let pattern = self.parse_pattern()?;

        // for in loop`
        self.expect_current(&Token::In)?;
        self.cursor += 1;

        let iterable = self.parse_expression(&[&Token::LeftCurly])?;

        let body = self.parse_body()?;

        Ok(Statement::ForLoop(pattern, iterable, body))
    }

    pub fn parse_while_loop(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::While)?;
        self.cursor += 1;

        let condition = self.parse_expression(&[&Token::LeftCurly])?;

        let body = self.parse_body()?;

        Ok(Statement::WhileLoop(condition, body))
    }

    pub fn parse_loop(&mut self) -> Result<Statement<'a>> {
        self.expect_current(&Token::Loop)?;
        self.cursor += 1;

        self.expect_current(&Token::LeftCurly)?;
        let body = self.parse_body()?;

        Ok(Statement::Loop(body))
    }

    pub fn parse_possible_left_paren_expressions(
        &mut self,
        expression_stack: &mut Vec<ExpressionSpan<'a>>,
    ) -> Result<Expression<'a>> {
        let expression = match expression_stack.last() {
            None => {
                // tuple or contained expression
                let tuple = self.parse_value_expression()?;
                tuple
            }
            Some(ExpressionSpan {
                expression:
                    Expression::VariableReference(_)
                    | Expression::AccessProperty(_, _)
                    | Expression::AccessIndex(_, _),
                ..
            }) => {
                // function call
                let left_side = pop_expression_stack!(self, expression_stack).expression;

                let mut args = Vec::new();

                self.cursor += 1;
                loop {
                    if matches!(self.current()?, Token::RightParen) {
                        break;
                    }

                    args.push(self.parse_expression(&[&Token::RightParen, &Token::Comma])?);

                    self.expect_either_current(&[&Token::Comma, &Token::RightParen])?;
                    if matches!(self.current()?, Token::Comma) {
                        self.cursor += 1;
                    }
                }
                self.cursor += 1;

                Expression::FunctionCall(FunctionCallExpression(left_side.into(), args.into()))
            }
            _ => {
                let expression = self.parse_expression(&[&Token::RightParen]);
                // still possibly a tuple
                if let Ok(expression) = expression {
                    expression
                } else {
                    // contained expression
                    self.cursor += 1;
                    let expression = self.parse_expression(&[&Token::RightParen])?;
                    self.cursor += 1;
                    Expression::ContainedExpression(expression.into())
                }
            }
        };

        Ok(expression)
    }

    pub fn parse_order_comparison_expression(
        &mut self,
        expression_stack: &mut Vec<ExpressionSpan<'a>>,
        expected_end: &[&Token],
    ) -> Result<Expression<'a>> {
        let token = self.current()?;
        let comparison: OrderComparison = token.try_into()?;

        let left_side = pop_expression_stack!(self, expression_stack).expression;
        self.cursor += 1;
        let right_side = self.parse_expression(expected_end)?;

        Ok(Expression::OrderComparison(OrderComparisonExpression(
            left_side.into(),
            right_side.into(),
            comparison,
        )))
    }

    pub fn parse_math_operation_expression(
        &mut self,
        expression_stack: &mut Vec<ExpressionSpan<'a>>,
        expected_end: &[&Token],
    ) -> Result<Expression<'a>> {
        let token = self.current()?;
        let operation: MathOperation = token.try_into()?;

        let left_side = pop_expression_stack!(self, expression_stack).expression;
        self.cursor += 1;
        let right_side = self.parse_expression(expected_end)?;

        Ok(Expression::MathOperation(MathOperationExpression(
            left_side.into(),
            right_side.into(),
            operation,
        )))
    }

    pub fn parse_math_operation_assignment_expression(
        &mut self,
        expression_stack: &mut Vec<ExpressionSpan<'a>>,
        expected_end: &[&Token],
    ) -> Result<Expression<'a>> {
        let token = self.current()?;
        let operation: MathOperation = token.try_into()?;

        let left_side = pop_expression_stack!(self, expression_stack).expression;

        let Expression::VariableReference(identifier) = left_side else {
            bail!(errors::InvalidLeftHandSideAssignment {
                dbg_line: at_dbg_line!(),
                src: self.code.to_string(),
                left_side: format!("{:?}", left_side),
                position: {
                    let span = self.current_span()?;
                    (span.from..span.to).into()
                },
            })
        };

        self.cursor += 1;
        let right_side = self.parse_expression(expected_end)?;

        Ok(Expression::VariableAssignment(
            identifier,
            Expression::MathOperation(MathOperationExpression(
                left_side.into(),
                right_side.into(),
                operation,
            ))
            .into(),
        ))
    }

    pub fn parse_unknown_float_expression(&mut self) -> Result<Expression<'a>> {
        let token @ Token::UnknownFloat(value) = self.current()? else {
            unreachable!()
        };

        let suggested_type = self.type_suggestions.last().unwrap_or(&Type::Unknown);
        let expression = match suggested_type {
            Type::Float32 => {
                let value = value.parse::<f32>();
                self.cursor += 1;
                match value {
                    Ok(value) => Expression::Float32(value),
                    Err(err) => bail!(err),
                }
            }
            Type::Float64 => {
                let value = value.parse::<f64>();
                self.cursor += 1;
                match value {
                    Ok(value) => Expression::Float64(value),
                    Err(err) => bail!(err),
                }
            }
            _ => {
                bail!(errors::MissingTypeAnnotations {
                    dbg_line: at_dbg_line!(),
                    token: format!("{:?}", token),
                    src: self.code.to_string(),
                    position: {
                        let span = self.current_span()?;
                        (span.from..span.to).into()
                    },
                })
            }
        };

        Ok(expression)
    }

    pub fn parse_unknown_integer_expression(&mut self) -> Result<Expression<'a>> {
        let token @ Token::UnknownInteger(value) = self.current()? else {
            unreachable!()
        };

        let suggested_type = self.type_suggestions.last().unwrap_or(&Type::Unknown);
        let expression = match suggested_type {
            Type::Integer32 => {
                let value = value.parse::<i32>();
                self.cursor += 1;
                match value {
                    Ok(value) => Expression::Integer32(value),
                    Err(err) => bail!(err),
                }
            }
            Type::Integer64 => {
                let value = value.parse::<i64>();
                self.cursor += 1;
                match value {
                    Ok(value) => Expression::Integer64(value),
                    Err(err) => bail!(err),
                }
            }

            _ => bail!(errors::MissingTypeAnnotations {
                dbg_line: at_dbg_line!(),
                token: format!("{:?}", token),
                src: self.code.to_string(),
                position: {
                    let span = self.current_span()?;
                    (span.from..span.to).into()
                },
            }),
        };

        Ok(expression)
    }
}
