use crate::{expression::Expression, value_type::Type};
use shared::interner::InternedString;

#[derive(Debug)]
#[repr(u8)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub identifier: InternedString,
    pub mutable: bool,
    pub value_type: Type,
    pub value: Expression,
}

#[derive(Debug)]
pub struct StructDeclarationField {
    pub identifier: InternedString,
    pub value_type: Type,
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub identifier: InternedString,
    pub fields: Box<[StructDeclarationField]>,
}

#[derive(Debug)]
#[repr(transparent)]
pub struct EnumDeclarationField {
    pub value_type: Type,
}

#[derive(Debug)]
pub struct EnumDeclarationVariant {
    pub identifier: InternedString,
    pub fields: Option<Box<[EnumDeclarationField]>>,
    pub discriminant: Option<Expression>,
}

#[derive(Debug)]
pub struct EnumDeclaration {
    pub identifier: InternedString,
    pub variants: Box<[EnumDeclarationVariant]>,
}
