mod error;
use error::{LexerError, UnexpectedCharacter, UnexpectedEOF, UnsupportedNumberSuffix};

mod token;
pub use token::{NumberPrefix, NumberSuffix, Token, TokenKind};

use shared::{dbg_line, span, span::Span};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    code: &'src str,
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

impl<'src> Lexer<'src> {
    pub fn new(code: &'src str) -> Self {
        Lexer {
            code,
            chars: code.chars().collect(),
            cursor: 0,
            errors: Vec::new(),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        let mut tokens = Vec::new();

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

        // TODO: Try to recover from error and continue lexing
        // The idea is to try finding as many errors as possible in one go (if possible)
        if self.cursor < self.chars.len() {
            let start = self.cursor;
            self.cursor = self.chars.len();
            Token(
                TokenKind::Garbage(Some(self.chars[start..].iter().collect())),
                span!(start, self.cursor),
            )
        } else {
            Token(
                TokenKind::Garbage(None),
                span!(self.cursor, self.chars.len()),
            )
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

        let span: Span = span!(start, self.cursor);

        match string.as_str() {
            "let" => Token(TokenKind::Let, span),
            "mut" => Token(TokenKind::Mut, span),
            "fun" => Token(TokenKind::Fun, span),
            "return" => Token(TokenKind::Return, span),
            "bail" => Token(TokenKind::Bail, span),
            "if" => Token(TokenKind::If, span),
            "else" => Token(TokenKind::Else, span),
            "loop" => Token(TokenKind::Loop, span),
            "while" => Token(TokenKind::While, span),
            "for" => Token(TokenKind::For, span),
            "in" => Token(TokenKind::In, span),
            "break" => Token(TokenKind::Break, span),
            "continue" => Token(TokenKind::Continue, span),
            "match" => Token(TokenKind::Match, span),
            "import" => Token(TokenKind::Import, span),
            "export" => Token(TokenKind::Export, span),
            _ => Token(TokenKind::Identifier(string), span),
        }
    }

    // TODO: Float
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
        Token(
            TokenKind::Integer(NumberPrefix::None, self.lex_number_suffix(), string),
            span!(start, self.cursor),
        )
    }

    pub fn lex_octal_number(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip 0o
        let string = string_with_match_pattern!(self, '0'..='7');
        Token(
            TokenKind::Integer(NumberPrefix::Octal, self.lex_number_suffix(), string),
            span!(start, self.cursor),
        )
    }

    pub fn lex_hexadecimal_number(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip 0x
        let string = string_with_match_pattern!(self, '0'..='9' | 'a'..='f' | 'A'..='F');
        Token(
            TokenKind::Integer(NumberPrefix::Hexadecimal, self.lex_number_suffix(), string),
            span!(start, self.cursor),
        )
    }

    pub fn lex_binary_number(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2; // Skip 0b
        let string = string_with_match_pattern!(self, '0' | '1');
        Token(
            TokenKind::Integer(NumberPrefix::Binary, self.lex_number_suffix(), string),
            span!(start, self.cursor),
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
                            position: span!(start, self.cursor),
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

        Token(TokenKind::String(string), span!(start, self.cursor))
    }

    pub fn lex_char(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1; // Skip '

        match (self.peek(), self.peek_next(1)) {
            (Some(ch), Some('\'')) => {
                let ch = *ch;
                self.cursor += 2;
                Token(TokenKind::Char(ch), span!(start, self.cursor))
            }
            (_, Some(end)) => self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
                dbg_line: dbg_line!(),
                actual: *end,
                expected: '\'',
                position: span!(start + 2, self.cursor + 2),
                src: self.code.to_string(),
            })),
            _ => self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
                dbg_line: dbg_line!(),
                expected: '\'',
                position: span!(start, self.cursor),
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

        Token(TokenKind::LineComment(string), span!(start, self.cursor))
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

        Token(TokenKind::BlockComment(string), span!(start, self.cursor))
    }

    pub fn lex_divide_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::DivideAssign, span!(start, self.cursor))
    }

    pub fn lex_divide(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Divide, span!(start, self.cursor))
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
        Token(TokenKind::MultiplyAssign, span!(start, self.cursor))
    }

    pub fn lex_multiply(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Multiply, span!(start, self.cursor))
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
        Token(TokenKind::AddAssign, span!(start, self.cursor))
    }

    pub fn lex_add(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Add, span!(start, self.cursor))
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
        Token(TokenKind::RightArrow, span!(start, self.cursor))
    }

    pub fn lex_subtract_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::SubtractAssign, span!(start, self.cursor))
    }

    pub fn lex_subtract(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Subtract, span!(start, self.cursor))
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
        Token(TokenKind::ModuloAssign, span!(start, self.cursor))
    }

    pub fn lex_modulo(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Modulo, span!(start, self.cursor))
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
        Token(TokenKind::Equals, span!(start, self.cursor))
    }

    pub fn lex_assign(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Assign, span!(start, self.cursor))
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
        Token(TokenKind::GreaterThan, span!(start, self.cursor))
    }

    pub fn lex_greater_than_or_equal(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::GreaterThanOrEqual, span!(start, self.cursor))
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
        Token(TokenKind::LeftArrow, span!(start, self.cursor))
    }

    pub fn lex_less_than_or_equal(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::LessThanOrEqual, span!(start, self.cursor))
    }

    pub fn lex_less_than(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::LessThan, span!(start, self.cursor))
    }

    pub fn lex_left_curly_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::LeftCurly, span!(start, self.cursor))
    }

    pub fn lex_right_curly_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::RightCurly, span!(start, self.cursor))
    }

    pub fn lex_left_square_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::LeftSquare, span!(start, self.cursor))
    }

    pub fn lex_right_square_bracket(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::RightSquare, span!(start, self.cursor))
    }

    pub fn lex_left_parenthesis(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::LeftParen, span!(start, self.cursor))
    }

    pub fn lex_right_parenthesis(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::RightParen, span!(start, self.cursor))
    }

    pub fn lex_question_mark(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Bail, span!(start, self.cursor))
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
        Token(TokenKind::NotEquals, span!(start, self.cursor))
    }

    pub fn lex_nand(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::Nand, span!(start, self.cursor))
    }

    pub fn lex_nor(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::Nor, span!(start, self.cursor))
    }

    pub fn lex_not(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Not, span!(start, self.cursor))
    }

    pub fn lex_ampersand(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        match self.peek() {
            Some('&') => self.lex_and(),
            Some(_) => Token(TokenKind::Ampersand, span!(start, self.cursor)),
            None => self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
                dbg_line: dbg_line!(),
                expected: '&',
                position: span!(self.cursor, self.cursor + 1),
                src: self.code.to_string(),
            })),
        }
    }

    pub fn lex_and(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::And, span!(start, self.cursor))
    }

    pub fn lex_pipe(&mut self) -> Token {
        match self.peek_next(1) {
            Some('|') => self.lex_or(),
            Some(ch) => self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
                dbg_line: dbg_line!(),
                actual: *ch,
                expected: '|',
                position: span!(self.cursor, self.cursor + 1),
                src: self.code.to_string(),
            })),
            None => self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
                dbg_line: dbg_line!(),
                expected: '|',
                position: span!(self.cursor, self.cursor + 1),
                src: self.code.to_string(),
            })),
        }
    }

    pub fn lex_or(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::Or, span!(start, self.cursor))
    }

    pub fn lex_dot(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        match self.peek() {
            Some('.') => {
                self.cursor += 1;
                Token(TokenKind::DoubleDot, span!(start, self.cursor))
            }
            _ => Token(TokenKind::Dot, span!(start, self.cursor)),
        }
    }

    pub fn lex_colon(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        match self.peek() {
            Some(':') => {
                self.cursor += 1;
                Token(TokenKind::DoubleColon, span!(start, self.cursor))
            }
            _ => Token(TokenKind::Colon, span!(start, self.cursor)),
        }
    }

    pub fn lex_comma(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Comma, span!(start, self.cursor))
    }

    pub fn lex_semicolon(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Semicolon, span!(start, self.cursor))
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
