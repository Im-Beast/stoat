use miette::{bail, Result};

use compiler::Compiler;
use lexer::Lexer;

pub fn run_file(file_path: &str, debug: bool) -> Result<()> {
    let code = std::fs::read_to_string(file_path);
    let code = match code {
        Ok(code) => code,
        Err(error) => bail!(error),
    };
    run(&code, debug)
}

pub fn run(code: &str, debug: bool) -> Result<()> {
    let lexer = Lexer::new(code);
    let result = lexer.lex();
    if !result.errors.is_empty() {
        for error in result.errors {
            eprintln!("Lexing errors: \n{:?}", error.into_err_report());
        }
        bail!("Failed to lex the code");
    }

    if debug {
        println!("Tokens:\n{:#?}", result.tokens);
    }

    let compiler = Compiler::new(result.tokens, code);
    let result = compiler.compile();
    if !result.errors.is_empty() {
        for error in result.errors {
            eprintln!("Lexing errors: \n{:?}", error.into_err_report());
        }
        bail!("Failed to lex the code");
    }

    Ok(())
}
