mod matchers;

use crate::shared::types::Type;
use matchers::*;

use std::fmt::Debug;
use std::ops::Range;
use std::str;

use miette::{bail, miette, LabeledSpan, Report, Result, SourceSpan};

const EMPTY_STR: &str = "";

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Identifier(&'a str),

    UnknownInteger(&'a str),
    Integer32(i32),
    Integer64(i64),
    UnknownFloat(&'a str),
    Float32(f32),
    Float64(f64),
    Bool(bool),
    String(&'a str),

    Enum,
    Struct,

    // Brackets
    LeftCurly,    // {
    RightCurly,   // }
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]

    // Error handling operators
    Bang,         // !
    QuestionMark, // ?
    Pipe,         // |

    // Manipulation operators
    Add,      // +
    Subtract, // -
    Divide,   // /
    Multiply, // *
    Modulo,   // %

    // Assignment operators
    Assign,         // =
    AddAssign,      // +=
    SubtractAssign, // -=
    DivideAssign,   // /=
    MutliplyAssign, // *=
    ModuloAssign,   // %=
    // Order comparison operators
    GreaterThan,        // >
    GreaterThanOrEqual, // >=
    LessThan,           // <
    LessThanOrEqual,    // <=
    // Logical comparison operators
    And,  // &&
    Nand, // !&
    Or,   // ||
    Nor,  // !|
    // comparison operators
    Equal,    // ==
    NotEqual, // !=

    // Variables
    VariableDeclaration, // let <name> <mut> <type>
    Mutable,
    Type(&'a str),

    // Functions
    Function,                // fun
    ReturnType(Type),        // ->
    DubiousReturnType(Type), // ?>

    Return,
    Bail,

    // Object Access
    Access, // .

    // Statements
    If,
    Else,
    Match,

    // Loops
    For,            // for i in x { ... }
    While,          // while x { ... }
    Loop,           // loop { ... }
    In,             // for i in [1,2] { ... }
    ExclusiveRange, // for i in 1..5 { ... }
    InclusiveRange, // for i in 1..=5 { ... }
    Break,
    Continue,

    // Separators
    Semicolon, // ;
    Comma,     // , Element separators (arrays, tuples, objects)
    Colon,     // , Type separator

    // Comments
    LineComment(&'a str),

    // Token used when something really bad happened
    Unexpected,
}

impl<'a> Token<'a> {
    pub fn eq_type(&self, other: &Token) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

#[derive(PartialEq)]
pub struct TokenSpan<'a> {
    pub token: Token<'a>,
    pub from: usize,
    pub to: usize,
}

impl From<&TokenSpan<'_>> for SourceSpan {
    fn from(span: &TokenSpan) -> Self {
        (span.from..span.to).into()
    }
}

impl<'a> TokenSpan<'a> {
    pub fn new(from: usize, to: usize, token: Token<'a>) -> Self {
        TokenSpan {
            from,
            // adding one because miette takes range instead of inclusive range
            to: to + 1,
            token,
        }
    }

    pub fn report<T: ToString, Y: ToString>(&self, code: T, message: Y) -> Report {
        let token = &self.token;
        miette!(
            labels = vec![LabeledSpan::at(self.from..self.to, message.to_string())],
            "{token:?}",
        )
        .with_source_code(code.to_string())
    }
}

impl Debug for TokenSpan<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(self.token.fmt(f)?)
    }
}

pub struct Lexer<'a> {
    cursor: usize,
    input: &'a [u8],
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            cursor: 0,
            input: input.as_bytes(),
        }
    }

    pub fn current(&self) -> u8 {
        self.peek_next_n(0)
    }

    pub fn peek_next(&self) -> u8 {
        self.peek_next_n(1)
    }

    pub fn peek_next_n(&self, n: usize) -> u8 {
        if self.cursor + n >= self.input.len() {
            0
        } else {
            self.input[self.cursor + n]
        }
    }

    pub fn skip_whitespace(&mut self) {
        loop {
            if self.current().is_ascii_whitespace() {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    pub fn read_slice_within(&mut self, range: Range<usize>) -> Result<&'a str> {
        let slice = &self.input[range];

        if let Some(str) = str::from_utf8(slice).ok() {
            Ok(str)
        } else {
            bail!("Failed to parse slice as UTF-8: {:?}", slice)
        }
    }

    pub fn read_str_that_matches(&mut self, match_fn: Matcher) -> Result<&'a str> {
        let from = self.cursor;

        while match_fn(self.current(), &self.input[from..self.cursor]) {
            self.cursor += 1;
        }

        if from == self.cursor {
            Ok(EMPTY_STR)
        } else {
            let slice = &self.input[from..self.cursor];
            if let Some(str) = str::from_utf8(slice).ok() {
                self.cursor -= 1;
                Ok(str)
            } else {
                bail!("Failed to parse slice as UTF-8: {:?}", slice)
            }
        }
    }

    pub fn skip_matches(&mut self, match_fn: Matcher) {
        let from = self.cursor;
        while match_fn(self.current(), &self.input[from..self.cursor]) {
            self.cursor += 1;
        }
    }

    pub fn lex(mut self) -> Result<Vec<TokenSpan<'a>>> {
        let mut tokens: Vec<TokenSpan<'a>> = Vec::new();
        let mut parse_type = false;

        loop {
            self.skip_whitespace();
            let span_from = self.cursor;

            let token = match self.current() {
                _ if parse_type => {
                    let type_str = self.read_str_that_matches(VALUE_TYPE_MATCHER)?;
                    parse_type = false;
                    Token::Type(type_str)
                }

                0 => break,

                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let str = self.read_str_that_matches(IDENTIFIER_MATCHER)?;

                    match &*str {
                        "fun" => Token::Function,
                        "return" => Token::Return,
                        "bail" => Token::Bail,

                        "for" => Token::For,
                        "while" => Token::While,
                        "loop" => Token::Loop,
                        "in" => Token::In,
                        "break" => Token::Break,
                        "continue" => Token::Continue,

                        "if" => Token::If,
                        "else" => Token::Else,

                        "match" => Token::Match,

                        "false" => Token::Bool(false),
                        "true" => Token::Bool(true),

                        "let" => Token::VariableDeclaration,
                        "mut" => Token::Mutable,

                        "enum" => Token::Enum,
                        "struct" => Token::Struct,

                        _ => Token::Identifier(str),
                    }
                }

                b'0'..=b'9' => {
                    self.skip_whitespace();
                    let start = self.cursor;

                    self.skip_matches(NUMBER_MATCHER);

                    if self.current() == b'.' && self.peek_next().is_ascii_digit() {
                        self.cursor += 2;
                        self.skip_matches(NUMBER_MATCHER);

                        let float = self.read_slice_within(start..self.cursor)?;
                        let suffix = self.read_str_that_matches(IDENTIFIER_MATCHER)?;
                        let token = match suffix {
                            "f32" => Token::Float32({
                                if let Ok(float) = float.parse::<f32>() {
                                    float
                                } else {
                                    bail!("Failed to parse float: {:?}", float);
                                }
                            }),
                            "f64" => Token::Float64({
                                if let Ok(float) = float.parse::<f64>() {
                                    float
                                } else {
                                    bail!("Failed to parse float: {:?}", float);
                                }
                            }),
                            _ if suffix.is_empty() => {
                                self.cursor -= 1;
                                Token::UnknownFloat(float)
                            }
                            _ => bail!("Unknown float suffix: {:?}", suffix),
                        };

                        token
                    } else {
                        let number = self.read_slice_within(start..self.cursor)?;
                        let suffix = self.read_str_that_matches(IDENTIFIER_MATCHER)?;
                        let token = match suffix {
                            "i32" => Token::Integer32({
                                if let Ok(number) = number.parse::<i32>() {
                                    number
                                } else {
                                    bail!("Failed to parse number: {:?}", number);
                                }
                            }),
                            "i64" => Token::Integer64({
                                if let Ok(number) = number.parse::<i64>() {
                                    number
                                } else {
                                    bail!("Failed to parse number: {:?}", number);
                                }
                            }),
                            _ if suffix.is_empty() => {
                                self.cursor -= 1;
                                Token::UnknownInteger(number)
                            }
                            _ => bail!("Unknown integer suffix: {:?}", suffix),
                        };

                        token
                    }
                }

                b'{' => Token::LeftCurly,
                b'}' => Token::RightCurly,
                b'(' => Token::LeftParen,
                b')' => Token::RightParen,
                b'[' => Token::LeftBracket,
                b']' => Token::RightBracket,

                b',' => Token::Comma,
                b':' => {
                    parse_type = true;
                    Token::Colon
                }
                b';' => Token::Semicolon,

                b'"' => {
                    self.cursor += 1;
                    let str = self.read_str_that_matches(NOT_QUOTE_MATCHER)?;
                    self.cursor += 1;
                    Token::String(str)
                }

                b'+' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::AddAssign
                    }
                    _ => Token::Add,
                },

                b'-' => match self.peek_next() {
                    b'>' => {
                        self.cursor += 2;
                        self.skip_whitespace();

                        let return_type = self.read_str_that_matches(VALUE_TYPE_MATCHER)?;
                        self.cursor -= 1;

                        Token::ReturnType(return_type.try_into()?)
                    }
                    b'=' => {
                        self.cursor += 1;
                        Token::SubtractAssign
                    }
                    _ => Token::Subtract,
                },

                b'?' => match self.peek_next() {
                    b'>' => {
                        self.cursor += 2;
                        self.skip_whitespace();

                        let return_type = self.read_str_that_matches(VALUE_TYPE_MATCHER)?;
                        self.cursor -= 1;

                        Token::DubiousReturnType(return_type.try_into()?)
                    }
                    _ => Token::QuestionMark,
                },

                b'/' => match self.peek_next() {
                    b'/' => Token::LineComment(self.read_str_that_matches(NOT_NEWLINE_MATCHER)?),
                    b'=' => {
                        self.cursor += 1;
                        Token::DivideAssign
                    }
                    _ => Token::Divide,
                },

                b'*' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::MutliplyAssign
                    }
                    _ => Token::Multiply,
                },

                b'%' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::ModuloAssign
                    }
                    _ => Token::Modulo,
                },

                b'=' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::Equal
                    }
                    _ => Token::Assign,
                },

                b'>' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::GreaterThanOrEqual
                    }
                    _ => Token::GreaterThan,
                },

                b'<' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::LessThanOrEqual
                    }
                    _ => Token::LessThan,
                },

                b'&' => match self.peek_next() {
                    b'&' => {
                        self.cursor += 1;
                        Token::And
                    }
                    _ => Token::Unexpected,
                },

                b'|' => match self.peek_next() {
                    b'|' => {
                        self.cursor += 1;
                        Token::Or
                    }
                    _ => Token::Pipe,
                },

                b'!' => match self.peek_next() {
                    b'=' => {
                        self.cursor += 1;
                        Token::NotEqual
                    }
                    b'&' => {
                        self.cursor += 1;
                        Token::Nand
                    }
                    b'|' => {
                        self.cursor += 1;
                        Token::Nor
                    }
                    _ => Token::Bang,
                },

                b'.' => match self.peek_next() {
                    b'.' => {
                        self.cursor += 1;
                        match self.peek_next() {
                            b'=' => {
                                self.cursor += 1;
                                Token::InclusiveRange
                            }
                            _ => Token::ExclusiveRange,
                        }
                    }
                    _ => Token::Access,
                },

                char if char.is_ascii_whitespace() => {
                    self.cursor += 1;
                    continue;
                }

                char => todo!("{}", char::from(char)),
            };

            tokens.push(TokenSpan::new(span_from, self.cursor, token));

            self.cursor += 1;
        }

        Ok(tokens)
    }
}
