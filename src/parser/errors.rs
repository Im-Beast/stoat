use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

// TODO: Macro for easier creation of these errors

#[derive(Error, Debug, Diagnostic)]
#[error("syntax-error")]
#[diagnostic(
	code(parser::missing_end),
	help("[{}] Insert {} there", self.dbg_line, self.end),
)]
pub struct MissingEnd {
    pub dbg_line: String,
    pub end: String,

    #[source_code]
    pub src: String,
    #[label("End ({}) is missing right here", self.end)]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("nonempty-expression-stack")]
#[diagnostic(
	code(parser::nonempty_expression_stack),
	help("[{}] Most likely a semicolon is missing somewhere around the selected area", self.dbg_line),
)]
pub struct NonemptyExpressionStack {
    pub dbg_line: String,
    #[source_code]
    pub src: String,
    #[label("Expression stack is non empty")]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("empty-expression-stack")]
#[diagnostic(
	code(parser::empty_expression_stack),
	help("[{}] i forgor when this happens", self.dbg_line),
)]
pub struct EmptyExpressionStack {
    pub dbg_line: String,
    #[source_code]
    pub src: String,
    #[label("Expression stack is non empty")]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unexpected-token")]
#[diagnostic(
	code(parser::unexpected_token),
	help("[{}] Expected {}, got {} instead", self.dbg_line, self.expected, self.actual),
)]
pub struct UnexpectedToken {
    pub dbg_line: String,
    pub expected: String,
    pub actual: String,
    #[source_code]
    pub src: String,
    #[label("Unexpected token: {}", self.actual)]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unexpected-eof")]
#[diagnostic(
	code(parser::unexpected_eof),
	help("[{}] Unexpected end of file", self.dbg_line),
)]
pub struct UnexpectedEndOfFile {
    pub dbg_line: String,
    #[source_code]
    pub src: String,
    #[label("Unexpected end of file")]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("missing-type-annotations")]
#[diagnostic(
	code(parser::missing_type_annotations),
	help("[{}] Explicitely specify type here", self.dbg_line),
)]
pub struct MissingTypeAnnotations {
    pub dbg_line: String,
    pub token: String,
    #[source_code]
    pub src: String,
    #[label(
        "Cannot convert {:?} to a certain type without more information",
        token
    )]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid-lhs-assignment")]
#[diagnostic(
	code(parser::invalid_lhs_assignment),
	help("[{}] This needs to be a variable reference or a variable declaration", self.dbg_line),
)]
pub struct InvalidLeftHandSideAssignment {
    pub dbg_line: String,
    pub left_side: String,
    #[source_code]
    pub src: String,
    #[label("Cannot assign to {:?}", self.left_side)]
    pub position: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid-mutable-pattern")]
#[diagnostic(
	code(parser::invalid_mutable_pattern),
	help("[{}] mut keyword cannot occur right after itself", self.dbg_line),
)]
pub struct InvalidMutablePattern {
    pub dbg_line: String,
    #[source_code]
    pub src: String,
    #[label("Invalid mutable pattern")]
    pub position: SourceSpan,
}
