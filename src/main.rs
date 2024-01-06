mod cli;
mod lexer;
mod shared;
mod vm;

use std::process::ExitCode;

fn main() -> ExitCode {
    cli::parse_cli()
}
