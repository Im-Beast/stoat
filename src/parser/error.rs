use miette::{Diagnostic, ErrReport, SourceSpan};
use thiserror::Error;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEOF(UnexpectedEOF),
    UnexpectedToken(UnexpectedToken),
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
