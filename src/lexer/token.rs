use shared::span::Span;

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum NumberPrefix {
    None,        // (Decimal) default
    Binary,      // 0b
    Octal,       // 0o
    Hexadecimal, // 0x
}

impl From<NumberPrefix> for u32 {
    fn from(prefix: NumberPrefix) -> Self {
        match prefix {
            NumberPrefix::None => 10,
            NumberPrefix::Binary => 2,
            NumberPrefix::Octal => 8,
            NumberPrefix::Hexadecimal => 16,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum NumberSuffix {
    None,

    // Unsigned integers
    U8,  // 69u8
    U16, // 69u16
    U32, // 69u32
    U64, // 69u64

    // Signed integers
    I8,  // 69i8
    I16, // 69i16
    I32, // 69i32
    I64, // 69i64

    // Floating point numbers
    F32, // 69f32
    F64, // 69f64
}

impl fmt::Display for NumberSuffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "{{none}}"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
    Garbage(Option<String>),

    Identifier(String),

    // Comments
    LineComment(String),
    BlockComment(String),

    // Punctuation
    Semicolon,   // ;
    Colon,       // :
    DoubleColon, // ::
    Comma,       // ,
    Dot,         // .
    DoubleDot,   // ..
    Ampersand,   // &
    Pipe,        // |

    // Data types
    Integer(NumberPrefix, NumberSuffix, String),
    Float(NumberSuffix, String),
    String(String),
    Char(char),
    Bool(bool),

    // Brackets
    LeftCurly,   // {
    RightCurly,  // }
    LeftSquare,  // [
    RightSquare, // ]
    LeftParen,   // (
    RightParen,  // )

    // Keywords
    Let,
    Mut,
    Fun,
    Return,
    Bail,
    If,
    Else,
    Loop,
    While,
    For,
    In,
    Break,
    Continue,
    Match,
    Import,
    Export,
    Struct,
    Enum,

    // Math operators
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Modulo,   // %

    // Logical operators
    And,  // &&
    Nand, // !&
    Or,   // ||
    Nor,  // !|
    Not,  // !

    // Assignment operators
    Assign,         // =
    AddAssign,      // +=
    SubtractAssign, // -=
    MultiplyAssign, // *=
    DivideAssign,   // /=
    ModuloAssign,   // %=

    // Comparison operators
    Equals,             // ==
    NotEquals,          // !=
    LessThan,           // <
    LessThanOrEqual,    // <=
    GreaterThan,        // >
    GreaterThanOrEqual, // >=

    // Other operators
    LeftArrow,  // <-
    RightArrow, // ->
}

#[derive(Debug)]
pub struct Token(pub TokenKind, pub Span);
