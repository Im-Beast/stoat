use crate::statement::Statement;

#[derive(Debug)]
pub struct AST {
    body: Vec<Statement>,
}

impl Default for AST {
    fn default() -> Self {
        Self { body: Vec::new() }
    }
}
