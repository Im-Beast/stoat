mod shared;

mod cli;

mod interpreter;
mod lexer;
mod new_lexer;
mod parser;

use std::process::ExitCode;

fn main() {
    new_lexer::lex();
    // cli::parse_cli()
}
