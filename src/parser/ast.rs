use crate::statement::Statement;

#[derive(Debug)]
pub struct AST {
    pub body: Vec<Statement>,
}

impl Default for AST {
    fn default() -> Self {
        Self { body: Vec::new() }
    }
}
