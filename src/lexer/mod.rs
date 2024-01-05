mod errors;

use self::errors::{LexerError, UnexpectedCharacter, UnexpectedEOF, UnsupportedNumberSuffix};

macro_rules! dbg_line {
    () => {
        format!("{}:{}", file!(), line!())
    };
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Into<Span> for (usize, usize) {
    fn into(self) -> Span {
        Span {
            start: self.0,
            end: self.1,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum NumberPrefix {
    None,        // (Decimal) default
    Binary,      // 0b
    Octal,       // 0o
    Hexadecimal, // 0x
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Debug)]
pub enum Token {
    Garbage(Option<String>, Span),

    Identifier(String, Span),

    // Comments
    LineComment(String, Span),
    BlockComment(String, Span),

    // Punctuation
    Semicolon(Span),
    Colon(Span),
    DoubleColon(Span),
    Comma(Span),
    Dot(Span),
    DoubleDot(Span),

    // Data types
    Number(NumberPrefix, NumberSuffix, String, Span),
    String(String, Span),
    Char(char, Span),

    // Brackets
    LeftCurly(Span),
    RightCurly(Span),
    LeftSquare(Span),
    RightSquare(Span),
    LeftParen(Span),
    RightParen(Span),

    // Keywords
    Let(Span),
    Mut(Span),
    Fun(Span),
    Return(Span),
    Bail(Span),
    If(Span),
    Else(Span),
    Loop(Span),
    While(Span),
    For(Span),
    In(Span),
    Break(Span),
    Continue(Span),
    Match(Span),
    Import(Span),
    Export(Span),

    // Math operators
    Add(Span),      // +
    Subtract(Span), // -
    Multiply(Span), // *
    Divide(Span),   // /
    Modulo(Span),   // %

    // Logical operators
    And(Span),  // &&
    Nand(Span), // !&
    Or(Span),   // ||
    Nor(Span),  // !|
    Not(Span),  // !

    // Assignment operators
    Assign(Span),         // =
    AddAssign(Span),      // +=
    SubtractAssign(Span), // -=
    MultiplyAssign(Span), // *=
    DivideAssign(Span),   // /=
    ModuloAssign(Span),   // %=

    // Comparison operators
    Equals(Span),             // ==
    NotEquals(Span),          // !=
    LessThan(Span),           // <
    LessThanOrEqual(Span),    // <=
    GreaterThan(Span),        // >
    GreaterThanOrEqual(Span), // >=

    // Other operators
    LeftArrow(Span),  // <-
    RightArrow(Span), // ->
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    code: &'a str,
    chars: Box<[char]>,
    cursor: usize,
    errors: Vec<LexerError>,
}

#[derive(Debug)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexerError>,
}

macro_rules! string_with_match_pattern {
    ($lexer: expr, $pattern: pat) => {
        string_with_match_pattern!($lexer, String::new(), $pattern)
    };

    ($lexer: expr, $string: expr, $pattern: pat) => {{
        let mut string = $string;
        loop {
            match $lexer.peek() {
                Some(ch @ $pattern) => {
                    string.push(*ch);
                    $lexer.cursor += 1;
                }
                _ => break,
            }
        }
        string
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Lexer {
            code,
            chars: code.chars().collect(),
            cursor: 0,
            errors: vec![],
        }
    }

    pub fn lex(mut self) -> LexerResult {
        let mut tokens = vec![];

        while let Some(token) = self.next() {
            tokens.push(token);
        }

        LexerResult {
            tokens: tokens,
            errors: self.errors,
        }
    }

    pub fn error(&mut self, error: LexerError) -> Token {
        self.errors.push(error);

        if self.cursor < self.chars.len() {
            let start = self.cursor;
            self.cursor = self.chars.len();
            Token::Garbage(
                Some(self.chars[start..].iter().collect()),
                (start, self.cursor).into(),
            )
        } else {
            Token::Garbage(None, (self.cursor, self.chars.len()).into())
        }
    }

    pub fn peek(&self) -> Option<&char> {
        self.chars.get(self.cursor)
    }

    pub fn peek_next(&self, n: usize) -> Option<&char> {
        self.chars.get(self.cursor + n)
    }

    pub fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.cursor;
        let string = string_with_match_pattern!(self, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_');

        let span: Span = (start, self.cursor).into();

        match string.as_str() {
            "let" => Token::Let(span),
            "mut" => Token::Mut(span),
            "fun" => Token::Fun(span),
            "return" => Token::Return(span),
            "bail" => Token::Bail(span),
            "if" => Token::If(span),
            "else" => Token::Else(span),
            "loop" => Token::Loop(span),
            "while" => Token::While(span),
            "for" => Token::For(span),
            "in" => Token::In(span),
            "break" => Token::Break(span),
            "continue" => Token::Continue(span),
            "match" => Token::Match(span),
            "import" => Token::Import(span),
            "export" => Token::Export(span),
            _ => Token::Identifier(string, span),
        }
    }

    pub fn lex_number(&mut self) -> Token {
        match self.peek_next(1) {
            Some('b') => self.lex_binary_number(),      // Binary
            Some('o') => self.lex_octal_number(),       // Octal
            Some('x') => self.lex_hexadecimal_number(), // Hexadecimal
            _ => self.lex_decimal_number(),
        }
    }

    pub fn lex_decimal_number(&mut self) -> Token {
        let start = self.cursor;
        let string = string_with_match_pattern!(self, '0'..='9');

        Token::Number(
            NumberPrefix::None,
            self.lex_number_suffix(),
            string,
            (start, self.cursor).into(),
        )
    }

    pub fn lex_octal_number(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip 0o
        let string = string_with_match_pattern!(self, '0'..='7');
        Token::Number(
            NumberPrefix::Octal,
            self.lex_number_suffix(),
            string,
            (start, self.cursor).into(),
        )
    }

    pub fn lex_hexadecimal_number(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip 0x
        let string = string_with_match_pattern!(self, '0'..='9' | 'a'..='f' | 'A'..='F');
        Token::Number(
            NumberPrefix::Hexadecimal,
            self.lex_number_suffix(),
            string,
            (start, self.cursor).into(),
        )
    }

    pub fn lex_binary_number(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip 0b
        let string = string_with_match_pattern!(self, '0' | '1');
        Token::Number(
            NumberPrefix::Binary,
            self.lex_number_suffix(),
            string,
            (start, self.cursor).into(),
        )
    }

    pub fn lex_number_suffix(&mut self) -> NumberSuffix {
        let start = self.cursor;
        let suffix = string_with_match_pattern!(self, 'u' | 'i' | 'f');

        if suffix.is_empty() {
            NumberSuffix::None
        } else {
            let suffix = string_with_match_pattern!(self, suffix, '1'..='9');
            match suffix.as_str() {
                "u8" => NumberSuffix::U8,
                "u16" => NumberSuffix::U16,
                "u32" => NumberSuffix::U32,
                "u64" => NumberSuffix::U64,

                "i8" => NumberSuffix::I8,
                "i16" => NumberSuffix::I16,
                "i32" => NumberSuffix::I32,
                "i64" => NumberSuffix::I64,

                "f32" => NumberSuffix::F32,
                "f64" => NumberSuffix::F64,

                ch => {
                    self.error(LexerError::UnsupportedNumberSuffix(
                        UnsupportedNumberSuffix {
                            dbg_line: dbg_line!(),
                            suffix: ch.to_string(),
                            position: (start, self.cursor - start).into(),
                            src: self.code.to_string(),
                        },
                    ));

                    NumberSuffix::None
                }
            }
        }
    }

    pub fn lex_string(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1; // Skip "

        let mut string = String::new();
        loop {
            match self.peek() {
                Some('"') => {
                    self.cursor += 1;
                    break;
                }
                Some(ch) => {
                    string.push(*ch);
                    self.cursor += 1
                }
                None => break,
            }
        }

        Token::String(string, (start, self.cursor).into())
    }

    pub fn lex_char(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1; // Skip '

        match (self.peek(), self.peek_next(1)) {
            (Some(ch), Some('\'')) => {
                let ch = *ch;
                self.cursor += 2;
                Token::Char(ch, (start, self.cursor - start).into())
            }
            (_, Some(end)) => self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
                dbg_line: dbg_line!(),
                actual: *end,
                expected: '\'',
                position: (start + 2, self.cursor - start).into(),
                src: self.code.to_string(),
            })),
            _ => self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
                dbg_line: dbg_line!(),
                expected: '\'',
                position: (start, self.cursor - start).into(),
                src: self.code.to_string(),
            })),
        }
    }

    pub fn lex_fw_slash(&mut self) -> Token {
        match self.peek_next(1) {
            Some('/') => self.lex_line_comment(),
            Some('*') => self.lex_block_comment(),
            Some('=') => self.lex_divide_assign(),
            _ => self.lex_divide(),
        }
    }

    pub fn lex_line_comment(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip //

        let mut string = String::new();
        loop {
            match self.peek() {
                Some('\n') => {
                    self.cursor += 1;
                    break;
                }
                Some(ch) => {
                    string.push(*ch);
                    self.cursor += 1
                }
                None => break,
            }
        }

        Token::LineComment(string, (start, self.cursor).into())
    }

    pub fn lex_block_comment(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip /*

        let mut string = String::new();
        loop {
            match self.peek() {
                Some('*') => {
                    self.cursor += 1;
                    match self.peek() {
                        Some('/') => {
                            self.cursor += 1;
                            break;
                        }
                        _ => string.push('*'),
                    }
                }
                Some(ch) => {
                    string.push(*ch);
                    self.cursor += 1
                }
                None => break,
            }
        }

        Token::BlockComment(string, (start, self.cursor).into())
    }

    pub fn lex_divide_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::DivideAssign((start, self.cursor).into())
    }

    pub fn lex_divide(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Divide((start, self.cursor).into())
    }

    pub fn lex_asterisk(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_multiply_assign(),
            _ => self.lex_multiply(),
        }
    }

    pub fn lex_multiply_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::MultiplyAssign((start, self.cursor).into())
    }

    pub fn lex_multiply(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Multiply((start, self.cursor).into())
    }

    pub fn lex_plus(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_add_assign(),
            _ => self.lex_add(),
        }
    }

    pub fn lex_add_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::AddAssign((start, self.cursor).into())
    }

    pub fn lex_add(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Add((start, self.cursor).into())
    }

    // lex_minus with methods for lex_subtract and lex_subtract_assign
    pub fn lex_minus(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_subtract_assign(),
            Some('>') => self.lex_right_arrow(),
            _ => self.lex_subtract(),
        }
    }

    pub fn lex_right_arrow(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::RightArrow((start, self.cursor).into())
    }

    pub fn lex_subtract_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::SubtractAssign((start, self.cursor).into())
    }

    pub fn lex_subtract(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Subtract((start, self.cursor).into())
    }

    pub fn lex_percent(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_modulo_assign(),
            _ => self.lex_modulo(),
        }
    }

    pub fn lex_modulo_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::ModuloAssign((start, self.cursor).into())
    }

    pub fn lex_modulo(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Modulo((start, self.cursor).into())
    }

    pub fn lex_equals(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_equals_equals(),
            _ => self.lex_assign(),
        }
    }

    pub fn lex_equals_equals(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::Equals((start, self.cursor).into())
    }

    pub fn lex_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Assign((start, self.cursor).into())
    }

    pub fn lex_right_pointy_bracket(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_greater_than_or_equal(),
            _ => self.lex_greater_than(),
        }
    }

    pub fn lex_greater_than(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::GreaterThan((start, self.cursor).into())
    }

    pub fn lex_greater_than_or_equal(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::GreaterThanOrEqual((start, self.cursor).into())
    }

    pub fn lex_left_pointy_bracket(&mut self) -> Token {
        match self.peek() {
            Some('=') => self.lex_less_than_or_equal(),
            Some('-') => self.lex_left_arrow(),
            _ => self.lex_less_than(),
        }
    }

    pub fn lex_left_arrow(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::LeftArrow((start, self.cursor).into())
    }

    pub fn lex_less_than_or_equal(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::LessThanOrEqual((start, self.cursor).into())
    }

    pub fn lex_less_than(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::LessThan((start, self.cursor).into())
    }

    pub fn lex_left_curly_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::LeftCurly((start, self.cursor).into())
    }

    pub fn lex_right_curly_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::RightCurly((start, self.cursor).into())
    }

    pub fn lex_left_square_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::LeftSquare((start, self.cursor).into())
    }

    pub fn lex_right_square_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::RightSquare((start, self.cursor).into())
    }

    pub fn lex_left_parenthesis(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::LeftParen((start, self.cursor).into())
    }

    pub fn lex_right_parenthesis(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::RightParen((start, self.cursor).into())
    }

    pub fn lex_question_mark(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Bail((start, self.cursor).into())
    }

    // lex_exclamation_mark with methods for lex_not and lex_not_equals
    pub fn lex_exclamation_mark(&mut self) -> Token {
        match self.peek_next(1) {
            Some('=') => self.lex_not_equals(),
            Some('&') => self.lex_nand(),
            Some('|') => self.lex_nor(),
            _ => self.lex_not(),
        }
    }

    pub fn lex_not_equals(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::NotEquals((start, self.cursor).into())
    }

    pub fn lex_nand(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::Nand((start, self.cursor).into())
    }

    pub fn lex_nor(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::Nor((start, self.cursor).into())
    }

    pub fn lex_not(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Not((start, self.cursor).into())
    }

    pub fn lex_ampersand(&mut self) -> Token {
        match self.peek_next(1) {
            Some('&') => self.lex_and(),
            Some(ch) => self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
                dbg_line: dbg_line!(),
                actual: *ch,
                expected: '&',
                position: (self.cursor, self.cursor + 1).into(),
                src: self.code.to_string(),
            })),
            None => self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
                dbg_line: dbg_line!(),
                expected: '&',
                position: (self.cursor, self.cursor + 1).into(),
                src: self.code.to_string(),
            })),
        }
    }

    pub fn lex_and(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::And((start, self.cursor).into())
    }

    pub fn lex_pipe(&mut self) -> Token {
        match self.peek_next(1) {
            Some('|') => self.lex_or(),
            Some(ch) => self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
                dbg_line: dbg_line!(),
                actual: *ch,
                expected: '|',
                position: (self.cursor, self.cursor + 1).into(),
                src: self.code.to_string(),
            })),
            None => self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
                dbg_line: dbg_line!(),
                expected: '|',
                position: (self.cursor, self.cursor + 1).into(),
                src: self.code.to_string(),
            })),
        }
    }

    pub fn lex_or(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token::Or((start, self.cursor).into())
    }

    pub fn lex_dot(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        match self.peek() {
            Some('.') => {
                self.cursor += 1;
                Token::DoubleDot((start, self.cursor).into())
            }
            _ => Token::Dot((start, self.cursor).into()),
        }
    }

    pub fn lex_colon(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        match self.peek() {
            Some(':') => {
                self.cursor += 1;
                Token::DoubleColon((start, self.cursor).into())
            }
            _ => Token::Colon((start, self.cursor).into()),
        }
    }

    pub fn lex_comma(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Comma((start, self.cursor).into())
    }

    pub fn lex_semicolon(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token::Semicolon((start, self.cursor).into())
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.peek()?;

        let token = match char {
            'a'..='z' | 'A'..='Z' => self.lex_ident_or_keyword(),
            '1'..='9' => self.lex_decimal_number(),
            '0' => self.lex_number(),
            '"' => self.lex_string(),
            '\'' => self.lex_char(),

            '/' => self.lex_fw_slash(),
            '*' => self.lex_asterisk(),
            '+' => self.lex_plus(),
            '-' => self.lex_minus(),
            '%' => self.lex_percent(),

            '>' => self.lex_right_pointy_bracket(),
            '<' => self.lex_left_pointy_bracket(),
            '=' => self.lex_equals(),
            '&' => self.lex_ampersand(),
            '|' => self.lex_pipe(),

            '?' => self.lex_question_mark(),
            '!' => self.lex_exclamation_mark(),

            '{' => self.lex_left_curly_bracket(),
            '}' => self.lex_right_curly_bracket(),
            '[' => self.lex_left_square_bracket(),
            ']' => self.lex_right_square_bracket(),
            '(' => self.lex_left_parenthesis(),
            ')' => self.lex_right_parenthesis(),

            '.' => self.lex_dot(),
            ':' => self.lex_colon(),
            ',' => self.lex_comma(),
            ';' => self.lex_semicolon(),

            ch if ch.is_whitespace() => {
                self.cursor += 1;
                return self.next();
            }
            _ => None?,
        };

        Some(token)
    }
}
