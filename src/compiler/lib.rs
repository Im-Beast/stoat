use std::collections::VecDeque;

use lexer::{LexerResult, NumberPrefix, NumberSuffix, Token, TokenKind};
use shared::{dbg_line, interner::Interner, span::Span};
use vm::{instruction::Instruction, program::Program, value::Value};

mod error;
use error::{CompilerError, UnexpectedEOF, UnexpectedToken};

mod value_type;
use value_type::Type;

#[derive(Debug)]
pub struct CompilerResult {
    pub errors: Vec<CompilerError>,
}

pub struct Compiler<'src> {
    code: &'src str,
    tokens: VecDeque<Token>,

    errors: Vec<CompilerError>,

    program: Program,
    interner: Interner,
}

macro_rules! consume_from {
    ($self: expr, [$($pat: pat),*]) => {
        match $self.consume_any() {
            token @ Token($($pat)|*, _span) => token,
            token =>  {
                $self.unexpected_token(&token.1, stringify!($($pat),*), format!("{:?}", token.0));
                return None
            },
        }
    };
}

macro_rules! consume {
    ($self: expr, $pat: pat) => {
        match $self.consume_any() {
            Token($pat, _) => {}
            token => {
                $self.unexpected_token(&token.1, stringify!($pat), format!("{:?}", token.0));
                return None;
            }
        }
    };

    ($self: expr, $pat: pat, $out: tt) => {
        match $self.consume_any() {
            Token($pat, _) => $out,
            token => {
                $self.unexpected_token(&token.1, stringify!($pat), format!("{:?}", token.0));
                return None;
            }
        }
    };

    (span; $self: expr, $pat: pat, $out: tt) => {
        match $self.consume_any() {
            Token($pat, span) => ($out, span),
            token @ Token(.., _span) => {
                $self.unexpected_token(&token.1, stringify!($pat), format!("{:?}", token.0));
                return None;
            }
        }
    };
}

macro_rules! push_instructions {
    ($self: expr, $($instr: expr),*) => {
        $($self.program.instructions.push($instr);)*
    };
}

impl<'src> Compiler<'src> {
    pub fn new(tokens: Vec<Token>, code: &'src str) -> Self {
        Self {
            code,
            tokens: VecDeque::from(tokens),

            errors: Vec::new(),

            program: Program::new(Vec::new()),
            interner: Interner::default(),
        }
    }

    pub fn error(&mut self, error: CompilerError) {
        self.errors.push(error);
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    pub fn peek_next(&self, n: usize) -> Option<&Token> {
        self.tokens.get(n)
    }

    pub fn consume_any(&mut self) -> Token {
        // TODO: change expect to unexpected eof error
        self.tokens.pop_front().expect("Tokens are empty")
    }

    pub fn unexpected_eof(&mut self, span: &Span, expected: &str) {
        self.error(CompilerError::UnexpectedEOF(UnexpectedEOF {
            dbg_line: dbg_line!(),
            expected: expected.to_string(),
            src: self.code.to_string(),
            position: span.into(),
        }))
    }

    pub fn unexpected_token(&mut self, span: &Span, expected: &str, actual: String) {
        self.error(CompilerError::UnexpectedToken(UnexpectedToken {
            dbg_line: dbg_line!(),
            expected: expected.to_string(),
            actual: actual.to_string(),
            src: self.code.to_string(),
            position: span.into(),
        }))
    }

    pub fn compile(mut self) -> CompilerResult {
        loop {
            if let None = self.compile_next() {
                break;
            }
        }

        CompilerResult {
            errors: self.errors,
        }
    }

    pub fn compile_value(&mut self, suggested_type: Option<&Type>) -> Option<Value> {
        let token = consume_from!(
            self,
            [
                TokenKind::Integer(..),
                TokenKind::Float(..),
                TokenKind::String(_),
                TokenKind::Char(_),
                TokenKind::Boolean(_)
            ]
        );

        let value = match token.0 {
            TokenKind::Integer(prefix, suffix, value) => {
                self.compile_integer(prefix, suffix, value, suggested_type)
            }
            TokenKind::Float(suffix, value) => self.compile_float(suffix, value, suggested_type),
            TokenKind::String(value) => Value::String(value),
            TokenKind::Char(value) => Value::Char(value),
            TokenKind::Boolean(value) => Value::Bool(value),
            _ => unreachable!(),
        };

        Some(value)
    }

    pub fn compile_integer(
        &mut self,
        prefix: NumberPrefix,
        mut suffix: NumberSuffix,
        value: String,
        suggested_type: Option<&Type>,
    ) -> Value {
        if suffix == NumberSuffix::None {
            suffix = match suggested_type {
                Some(value_type) => match value_type {
                    Type::I8 => NumberSuffix::I8,
                    Type::I16 => NumberSuffix::I16,
                    Type::I32 => NumberSuffix::I32,
                    Type::I64 => NumberSuffix::I64,

                    Type::U8 => NumberSuffix::U8,
                    Type::U16 => NumberSuffix::U16,
                    Type::U32 => NumberSuffix::U32,
                    Type::U64 => NumberSuffix::U64,

                    _ => unreachable!(),
                },

                // TODO: try to infer type instead
                None => NumberSuffix::I32,
            }
        }

        macro_rules! parse_int {
            ($repr: ident, $value_type: ident) => {{
                let value = $repr::from_str_radix(&value, prefix.into());
                Value::$value_type(value.unwrap())
            }};
        }

        match suffix {
            NumberSuffix::I8 => parse_int!(i8, I8),
            NumberSuffix::I16 => parse_int!(i16, I16),
            NumberSuffix::I32 => parse_int!(i32, I32),
            NumberSuffix::I64 => parse_int!(i64, I64),

            NumberSuffix::U8 => parse_int!(u8, U8),
            NumberSuffix::U16 => parse_int!(u16, U16),
            NumberSuffix::U32 => parse_int!(u32, U32),
            NumberSuffix::U64 => parse_int!(u64, U64),

            _ => unreachable!(),
        }
    }

    pub fn compile_float(
        &mut self,
        mut suffix: NumberSuffix,
        value: String,
        suggested_type: Option<&Type>,
    ) -> Value {
        if suffix == NumberSuffix::None {
            suffix = match suggested_type {
                Some(value_type) => match value_type {
                    Type::F32 => NumberSuffix::F32,
                    Type::F64 => NumberSuffix::F64,
                    _ => unreachable!(),
                },
                // TODO: try to infer type instead
                None => NumberSuffix::F32,
            }
        }

        macro_rules! parse_float {
            ($repr: ident, $value_type: ident) => {{
                let value = value.parse::<$repr>();
                Value::$value_type(value.unwrap())
            }};
        }

        match suffix {
            NumberSuffix::F32 => parse_float!(f32, F32),
            NumberSuffix::F64 => parse_float!(f64, F64),

            _ => unreachable!(),
        }
    }

    pub fn compile_type(&mut self) -> Option<Type> {
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
                        let value_type = self.compile_type()?;
                        Type::MutableReference(Box::new(value_type))
                    }
                    _ => {
                        let value_type = self.compile_type()?;
                        Type::Reference(Box::new(value_type))
                    }
                }
            }
            _ => unreachable!(),
        };

        Some(value_type)
    }

    pub fn compile_let(&mut self) -> Option<()> {
        consume!(self, TokenKind::Let); // let

        // let identifier ..;
        // or
        // let mut identifier ..;
        let token = consume_from!(self, [TokenKind::Identifier(_), TokenKind::Mut]);
        let (mutable, identifier) = match token.0 {
            TokenKind::Identifier(identifier) => (false, identifier),
            TokenKind::Mut => (
                true,
                consume!(self, TokenKind::Identifier(identifier), identifier),
            ),
            _ => unreachable!(),
        };

        // let identifier = value [implicit type from value]
        // or
        // let identifier: value_type = value [explicit type, force value to be of value_type]
        let token = consume_from!(self, [TokenKind::Equals, TokenKind::Colon]);
        let (value_type, value) = match &token.0 {
            TokenKind::Equals => {
                let value = self.compile_value(None)?;
                ((&value).into(), value)
            }
            TokenKind::Colon => {
                let value_type = self.compile_type()?;
                consume!(self, TokenKind::Equals);
                let value = self.compile_value(Some(&value_type))?;
                (value_type, value)
            }
            _ => unreachable!(),
        };

        // let identifier = value;
        // or
        // let identifier: value_type = value;
        consume!(self, TokenKind::Semicolon); // ;

        println!(
            "let {}{}: {:?} = {:?};",
            if mutable { "mut " } else { "" },
            identifier,
            value_type,
            value
        );

        let interned_ident = self.interner.intern(&identifier);
        push_instructions!(
            self,
            Instruction::Push(value),
            Instruction::Push(Value::Pointer(interned_ident)),
            Instruction::DeclareVariable
        );

        Some(())
    }

    pub fn compile_next(&mut self) -> Option<()> {
        let next = self.peek()?;

        match &next.0 {
            TokenKind::BlockComment(_) | TokenKind::LineComment(_) => {
                self.consume_any();
            }
            TokenKind::Let => self.compile_let()?,
            token => todo!("Token: {:?}", token),
        }

        Some(())
    }
}
