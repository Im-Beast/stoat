use std::collections::VecDeque;

use rustc_hash::FxHasher;

use crate::shared::name_bihasher::NameBihasher;

use super::statements::Statement;

#[derive(Debug)]
pub struct AST<'a> {
    pub body: VecDeque<Statement<'a>>,
    pub bihasher: NameBihasher<FxHasher>,
}
