use miette::{Diagnostic, ErrReport, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum LexerError {
    UnexpectedCharacter(UnexpectedCharacter),
    UnexpectedEOF(UnexpectedEOF),
    UnsupportedNumberSuffix(UnsupportedNumberSuffix),
}

impl LexerError {
    pub fn into_err_report(self) -> ErrReport {
        self.into()
    }
}

impl Into<ErrReport> for LexerError {
    fn into(self) -> ErrReport {
        match self {
            LexerError::UnexpectedCharacter(e) => ErrReport::from(e),
            LexerError::UnexpectedEOF(e) => ErrReport::from(e),
            LexerError::UnsupportedNumberSuffix(e) => ErrReport::from(e),
        }
    }
}

#[derive(Error, Debug, Diagnostic, Clone)]
#[error("unexpected-character")]
#[diagnostic(
	code(parser::unexpected_character),
	help("[{}] Expected {:?}, got {:?} instead", self.dbg_line, self.expected, self.actual),
)]
pub struct UnexpectedCharacter {
    pub dbg_line: String,
    pub expected: char,
    pub actual: char,
    #[source_code]
    pub src: String,
    #[label("Unexpected character: {:?}", self.actual)]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic, Clone)]
#[error("unexpected-eof")]
#[diagnostic(
	code(parser::unexpected_eof),
	help("[{}] Expected {}, got EOF instead", self.dbg_line, self.expected),
)]
pub struct UnexpectedEOF {
    pub dbg_line: String,
    pub expected: char,
    #[source_code]
    pub src: String,
    #[label("Unexpected EOF")]
    pub position: SourceSpan,
}

// unsupported number suffix
#[derive(Error, Debug, Diagnostic, Clone)]
#[error("unsupported-number-suffix")]
#[diagnostic(
    code(parser::unsupported_number_suffix),
    help("[{}] {:?} is not a valid number suffix", self.dbg_line, self.suffix),
)]
pub struct UnsupportedNumberSuffix {
    pub dbg_line: String,
    pub suffix: String,
    #[source_code]
    pub src: String,
    #[label("Unsupported number suffix: {:?}", self.suffix)]
    pub position: SourceSpan,
}
