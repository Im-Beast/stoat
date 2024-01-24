use miette::{bail, Result};

use lexer::Lexer;
use parser::Parser;

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

    let parser = Parser::new(result.tokens, code);
    let result = parser.parse();
    if !result.errors.is_empty() {
        for error in result.errors {
            eprintln!("Parsing errors: \n{:?}", error.into_err_report());
        }
        bail!("Failed to parse the code");
    }

    if debug {
        println!("AST:\n{:#?}", result.ast);
    }

    Ok(())
}
