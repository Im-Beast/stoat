mod shared;

mod cli;

mod interpreter;
mod lexer;
mod parser;

use std::process::ExitCode;

fn main() -> ExitCode {
    cli::parse_cli()
}
