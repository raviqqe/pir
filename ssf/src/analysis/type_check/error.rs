use crate::ir::*;
use crate::types::{self, Type};
use std::error::Error;
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeCheckError {
    ElementIndexOutOfBounds(RecordElement),
    ForeignDefinitionNotFound(ForeignDefinition),
    FunctionExpected(Expression),
    NoAlternativeFound(Case),
    TypeNotFound(types::Reference),
    TypesNotMatched(Type, Type),
    VariableNotFound(Variable),
    VariantInVariant(Variant),
    WrongElementCount(Expression),
}

impl Display for TypeCheckError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:#?}", self)
    }
}

impl Error for TypeCheckError {}
