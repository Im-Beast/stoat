use lexer::NumberSuffix;
use miette::{Diagnostic, ErrReport, SourceSpan};
use thiserror::Error;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEOF(UnexpectedEOF),
    UnexpectedToken(UnexpectedToken),
    InvalidFloatSuffix(InvalidFloatSuffix),
    InvalidIntegerSuffix(InvalidIntegerSuffix),
}

impl ParserError {
    pub fn into_err_report(self) -> ErrReport {
        self.into()
    }
}

impl Into<ErrReport> for ParserError {
    fn into(self) -> ErrReport {
        match self {
            ParserError::UnexpectedEOF(e) => ErrReport::from(e),
            ParserError::UnexpectedToken(e) => ErrReport::from(e),
            ParserError::InvalidFloatSuffix(e) => ErrReport::from(e),
            ParserError::InvalidIntegerSuffix(e) => ErrReport::from(e),
        }
    }
}

#[derive(Error, Debug, Diagnostic, Clone)]
#[error("unexpected-eof")]
#[diagnostic(
	code(parser::unexpected_eof),
	help("[{}] Expected {}, got EOF instead", self.dbg_line, self.expected),
)]
pub struct UnexpectedEOF {
    pub dbg_line: String,
    pub expected: String,
    #[source_code]
    pub src: String,
    #[label("Unexpected EOF")]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unexpected-token")]
#[diagnostic(
	code(parser::unexpected_character),
	help("[{}] Expected {:?}, got {:?} instead", self.dbg_line, self.expected, self.actual),
)]
pub struct UnexpectedToken {
    pub dbg_line: String,
    pub expected: String,
    pub actual: String,
    #[source_code]
    pub src: String,
    #[label("Unexpected token: {:?}", self.actual)]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid-float-suffix")]
#[diagnostic(
    code(parser::invalid_float_suffix),
    help(
        "[{}] Invalid float suffix: {}.
        Possible float suffixes are: f32, f64",
        self.dbg_line, self.suffix
    ),
)]
pub struct InvalidFloatSuffix {
    pub dbg_line: String,
    pub suffix: NumberSuffix,
    #[source_code]
    pub src: String,
    #[label("Invalid float suffix: {}", self.suffix)]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid-integer-suffix")]
#[diagnostic(
    code(parser::invalid_integer_suffix),
    help(
        "[{}] Invalid integer suffix: {}.
        Possible integer suffixes are: u8, u16, u32, u64, i8, i16, i32, i64",
        self.dbg_line, self.suffix
    ),
)]
pub struct InvalidIntegerSuffix {
    pub dbg_line: String,
    pub suffix: NumberSuffix,
    #[source_code]
    pub src: String,
    #[label("Invalid integer suffix: {}", self.suffix)]
    pub position: SourceSpan,
}
