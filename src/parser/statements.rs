use super::{expressions::Expression, patterns::Pattern};
use crate::shared::{name_bihasher::NameHash, types::Type};

// (arg_name, arg_type)
pub type FunctionArg<'a> = (NameHash, Type);
//
pub type StatementBody<'a> = Box<[Statement<'a>]>;
// (variant_identifier, variant_discriminant, field_type)
pub type EnumVariant<'a> = (&'a str, Expression<'a>, Option<Type>);
// (field_key, field_type)
pub type StructField<'a> = (&'a str, Type);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    // identifier, args, return_type, body
    Function(NameHash, Box<[FunctionArg<'a>]>, Type, StatementBody<'a>),

    // for <pattern> in <expression> { <body> }
    ForLoop(Pattern, Expression<'a>, StatementBody<'a>),
    // while <expression> { <body> }
    WhileLoop(Expression<'a>, StatementBody<'a>),
    // for { <body> }
    Loop(StatementBody<'a>),

    Continue,
    Break,
    Return(Expression<'a>),
    Bail(Expression<'a>),

    // identifier, is_mutable, type, value
    VariableDeclaration(Pattern, bool, Type, Expression<'a>),

    // TODO: support enums with fields
    // identifier, variants
    EnumDeclaration(NameHash, Vec<EnumVariant<'a>>),

    // identifier, fields
    StructDeclaration(NameHash, Vec<StructField<'a>>),

    // expression
    Expression(Expression<'a>),
}
