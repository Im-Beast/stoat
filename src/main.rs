mod cli;
mod lexer;
mod shared;

use std::process::ExitCode;

fn main() -> ExitCode {
    cli::parse_cli()
}
