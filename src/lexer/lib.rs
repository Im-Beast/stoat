mod error;
use std::collections::VecDeque;

use error::{LexerError, UnexpectedCharacter, UnexpectedEOF, UnsupportedNumberSuffix};

mod token;
pub use token::{NumberPrefix, NumberSuffix, Token, TokenKind};

use shared::{dbg_line, span};

// For future lex thingies:
// If a token is consisting of more than 1 character:
//
// Create a `lex_char_name` method which:
//  - Uses peek_next(1) to determine the next character
//  - Matches that peeked character to determine what
//    needs to be lexed and calls the `lex_actual_token` method.
//    (if `char_name` is the same as `token` name, just name it the same with `_token` suffix (e.g. `lex_dot_token`))
//  - Only the `lex_actual_token` method should mutate the cursor.

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    code: &'src str,
    chars: Box<[char]>,
    cursor: usize,
    errors: Vec<LexerError>,
}

#[derive(Debug)]
pub struct LexerResult {
    pub tokens: VecDeque<Token>,
    pub errors: Vec<LexerError>,
}

macro_rules! unexpected_eof {
    ($self:expr, $start:expr,  $expected:expr) => {
        $self.error(LexerError::UnexpectedEOF(UnexpectedEOF {
            dbg_line: dbg_line!(),
            expected: $expected,
            position: span!($start, $self.cursor),
            src: $self.code.to_string(),
        }))
    };
}

macro_rules! unexpected_char {
    ($self:expr, $start: expr, $actual:expr, $expected:expr) => {
        $self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
            dbg_line: dbg_line!(),
            expected: $expected,
            actual: $actual,
            position: span!($start, $self.cursor),
            src: $self.code.to_string(),
        }))
    };

    ($self:expr, $start:expr, $end:expr, $actual:expr, $expected:expr) => {
        $self.error(LexerError::UnexpectedCharacter(UnexpectedCharacter {
            dbg_line: dbg_line!(),
            expected: $expected,
            actual: $actual,
            position: span!($start, $end),
            src: $self.code.to_string(),
        }))
    };
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
        let mut tokens = VecDeque::new();

        while let Some(token) = self.next() {
            tokens.push_back(token);
        }

        LexerResult {
            tokens: tokens,
            errors: self.errors,
        }
    }

    pub fn error(&mut self, error: LexerError) -> Token {
        self.errors.push(error);

        // TODO: Try to recover from error and continue lexing
        // The idea is to try finding as many errors as possible in one go (whenever possible)
        let start = self.cursor;

        let kind = if self.cursor < self.chars.len() {
            self.cursor = self.chars.len();
            TokenKind::Garbage(Some(self.chars[start..].iter().collect()))
        } else {
            TokenKind::Garbage(None)
        };

        Token(kind, span!(start, self.cursor))
    }

    pub fn peek(&self) -> Option<&char> {
        self.chars.get(self.cursor)
    }

    pub fn peek_next(&self, n: usize) -> Option<&char> {
        self.chars.get(self.cursor + n)
    }

    pub fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.cursor;
        let string = string_with_match_pattern!(
            self,
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_'
        );

        let kind = match string.as_str() {
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "fun" => TokenKind::Fun,
            "return" => TokenKind::Return,
            "bail" => TokenKind::Bail,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "loop" => TokenKind::Loop,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "match" => TokenKind::Match,
            "import" => TokenKind::Import,
            "export" => TokenKind::Export,
            "true" => TokenKind::Bool(true),
            "false" => TokenKind::Bool(false),
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            _ => TokenKind::Identifier(string),
        };

        Token(kind, span!(start, self.cursor))
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
        let mut string = string_with_match_pattern!(self, '0'..='9');

        match (self.peek(), self.peek_next(1)) {
            (Some('.'), Some('0'..='9')) => {
                self.cursor += 1;
                string.push('.');
                let string = string_with_match_pattern!(self, string, '0'..='9');

                Token(
                    TokenKind::Float(self.lex_number_suffix(), string),
                    span!(start, self.cursor),
                )
            }
            _ => Token(
                TokenKind::Integer(NumberPrefix::None, self.lex_number_suffix(), string),
                span!(start, self.cursor),
            ),
        }
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
        self.cursor += 1;
        match (self.peek(), self.peek_next(1)) {
            (Some(ch), Some('\'')) => {
                let ch = *ch;
                self.cursor += 2;
                Token(TokenKind::Char(ch), span!(start, self.cursor))
            }
            (_, Some(end)) => unexpected_char!(self, start + 2, self.cursor + 2, *end, '\''),
            _ => unexpected_eof!(self, start, '\''),
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
                Some('*') => match self.peek_next(1) {
                    Some('/') => {
                        self.cursor += 2;
                        break;
                    }
                    _ => {
                        string.push('*');
                        self.cursor += 1;
                    }
                },
                Some(ch) => {
                    string.push(*ch);
                    self.cursor += 1;
                }
                _ => {
                    unexpected_eof!(self, start, '*');
                    break;
                }
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
        match self.peek_next(1) {
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
        match self.peek_next(1) {
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

    pub fn lex_minus(&mut self) -> Token {
        match self.peek_next(1) {
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
        match self.peek_next(1) {
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
        match self.peek_next(1) {
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
        match self.peek_next(1) {
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
        match self.peek_next(1) {
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
        match self.peek_next(1) {
            Some('&') => self.lex_and(),
            _ => self.lex_ampersand_token(),
        }
    }

    pub fn lex_ampersand_token(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Ampersand, span!(start, self.cursor))
    }

    pub fn lex_and(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::And, span!(start, self.cursor))
    }

    pub fn lex_pipe(&mut self) -> Token {
        match self.peek_next(1) {
            Some('|') => self.lex_or(),
            _ => self.lex_pipe_token(),
        }
    }

    pub fn lex_pipe_token(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Pipe, span!(start, self.cursor))
    }

    pub fn lex_or(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::Or, span!(start, self.cursor))
    }

    pub fn lex_dot(&mut self) -> Token {
        match self.peek_next(1) {
            Some('.') => match self.peek_next(2) {
                Some('=') => self.lex_double_dot_equals(),
                _ => self.lex_double_dot(),
            },
            _ => self.lex_dot_token(),
        }
    }

    pub fn lex_double_dot_equals(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 3;
        Token(TokenKind::DoubleDotEquals, span!(start, self.cursor))
    }

    pub fn lex_double_dot(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::DoubleDot, span!(start, self.cursor))
    }

    pub fn lex_dot_token(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Dot, span!(start, self.cursor))
    }

    pub fn lex_colon(&mut self) -> Token {
        match self.peek_next(1) {
            Some(':') => self.lex_double_colon(),
            _ => self.lex_colon_token(),
        }
    }

    pub fn lex_double_colon(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 2;
        Token(TokenKind::DoubleColon, span!(start, self.cursor))
    }

    pub fn lex_colon_token(&mut self) -> Token {
        let start = self.cursor;
        self.cursor += 1;
        Token(TokenKind::Colon, span!(start, self.cursor))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dont_lex_invalid_integers() {
        let invalid_suffixes = ["i", "u", "f", "i82", "i69", "u23"];
        for invalid_suffix in invalid_suffixes {
            let code = format!("{}{}", "123", invalid_suffix);

            let lexer = Lexer::new(&code);
            let errors = lexer.lex().errors;

            assert_eq!(errors.len(), 1);

            assert!(matches!(
                &errors[0],
                LexerError::UnsupportedNumberSuffix(UnsupportedNumberSuffix { .. })
            ));
        }
    }

    #[test]
    fn lex_other_tokens() {
        // The order is the same as in token.rs

        #[rustfmt::skip]
        let tokens = [
            ("identifier", TokenKind::Identifier("identifier".to_string())),

            ("//comment", TokenKind::LineComment("comment".to_string())),
            ("/*comment*/", TokenKind::BlockComment("comment".to_string())),
            
            (";", TokenKind::Semicolon),
            (":", TokenKind::Colon),
            ("::", TokenKind::DoubleColon),
            (",", TokenKind::Comma),
            (".", TokenKind::Dot),
            ("..", TokenKind::DoubleDot),
            ("..=", TokenKind::DoubleDotEquals),
            ("&", TokenKind::Ampersand),
            ("|", TokenKind::Pipe),

            ("69", TokenKind::Integer(NumberPrefix::None, NumberSuffix::None, "69".to_string())),
            ("69i32", TokenKind::Integer(NumberPrefix::None, NumberSuffix::I32, "69".to_string())),
            ("69u32", TokenKind::Integer(NumberPrefix::None, NumberSuffix::U32, "69".to_string())),
            ("69f32", TokenKind::Integer(NumberPrefix::None, NumberSuffix::F32, "69".to_string())),
            ("69.0", TokenKind::Float(NumberSuffix::None, "69.0".to_string())),
            ("69.0f32", TokenKind::Float(NumberSuffix::F32, "69.0".to_string())),
            ("69.0f64", TokenKind::Float(NumberSuffix::F64, "69.0".to_string())),

            ("\"string\"", TokenKind::String("string".to_string())),
            ("'c'", TokenKind::Char('c')),
            
            ("{", TokenKind::LeftCurly),
            ("}", TokenKind::RightCurly),
            ("[", TokenKind::LeftSquare),
            ("]", TokenKind::RightSquare),
            ("(", TokenKind::LeftParen),
            (")", TokenKind::RightParen),
            
            ("let", TokenKind::Let),
            ("mut", TokenKind::Mut),
            ("fun", TokenKind::Fun),
            ("return", TokenKind::Return),
            ("bail", TokenKind::Bail),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("loop", TokenKind::Loop),
            ("while", TokenKind::While),
            ("for", TokenKind::For),
            ("in", TokenKind::In),
            ("break", TokenKind::Break),
            ("continue", TokenKind::Continue),
            ("match", TokenKind::Match),
            ("import", TokenKind::Import),
            ("export", TokenKind::Export),
            ("true", TokenKind::Bool(true)),
            ("false", TokenKind::Bool(false)),
            ("struct", TokenKind::Struct),
            ("enum", TokenKind::Enum),

            ("+", TokenKind::Add),
            ("-", TokenKind::Subtract),
            ("*", TokenKind::Multiply),
            ("/", TokenKind::Divide),
            ("%", TokenKind::Modulo),

            ("&&", TokenKind::And),
            ("!&", TokenKind::Nand),
            ("||", TokenKind::Or),
            ("!|", TokenKind::Nor),
            ("!", TokenKind::Not),

            ("=", TokenKind::Assign),
            ("+=", TokenKind::AddAssign),
            ("-=", TokenKind::SubtractAssign),
            ("*=", TokenKind::MultiplyAssign),
            ("/=", TokenKind::DivideAssign),

            ("==", TokenKind::Equals),
            ("!=", TokenKind::NotEquals),
            ("<", TokenKind::LessThan),
            ("<=", TokenKind::LessThanOrEqual),
            (">", TokenKind::GreaterThan),
            (">=", TokenKind::GreaterThanOrEqual),

            ("<-", TokenKind::LeftArrow),
            ("->", TokenKind::RightArrow),
        ];

        for (code, expected) in tokens.iter() {
            let lexer = Lexer::new(code);
            let tokens = lexer.lex().tokens;

            assert_eq!(
                tokens.len(),
                1,
                "failed parsing {expected:?}, got left out tokens: {tokens:?}"
            );

            assert_eq!(
                &tokens[0].0, expected,
                "failed parsing {expected:?}, got {:?} instead",
                &tokens[0].0
            );
        }
    }
}
