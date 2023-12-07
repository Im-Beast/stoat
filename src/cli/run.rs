use miette::{bail, Result};

use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

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
    let tokens = match lexer.lex() {
        Ok(tokens) => {
            if debug {
                println!("Tokens:\n{tokens:?}");
            }
            tokens
        }
        Err(e) => bail!("Failed to lex code: {e:?}"),
    };

    let parser = Parser::new(code, tokens);
    let ast = match parser.parse() {
        Ok(ast) => {
            if debug {
                println!("AST:\n{ast:?}");
            }
            ast
        }
        Err(e) => bail!("Failed to parse code: {e:?}"),
    };

    let interpreter = Interpreter::new(ast);
    match interpreter.evaluate() {
        Ok(_) => {}
        Err(e) => bail!("Failed to interpret code: {e:?}"),
    }

    Ok(())
}
