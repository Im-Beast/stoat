use lexer::Token;
use miette::{Diagnostic, ErrReport, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum CompilerError {
    UnexpectedEOF(UnexpectedEOF),
    UnexpectedToken(UnexpectedToken),
}

impl CompilerError {
    pub fn into_err_report(self) -> ErrReport {
        self.into()
    }
}

impl Into<ErrReport> for CompilerError {
    fn into(self) -> ErrReport {
        match self {
            CompilerError::UnexpectedEOF(e) => ErrReport::from(e),
            CompilerError::UnexpectedToken(e) => ErrReport::from(e),
        }
    }
}

#[derive(Error, Debug, Diagnostic, Clone)]
#[error("unexpected-eof")]
#[diagnostic(
	code(compiler::unexpected_eof),
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

#[derive(Error, Debug, Diagnostic, Clone)]
#[error("unexpected-token")]
#[diagnostic(
	code(compiler::unexpected_character),
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
