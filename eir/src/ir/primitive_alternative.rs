use super::expression::Expression;
use super::primitive::Primitive;
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
pub struct PrimitiveAlternative {
    primitive: Primitive,
    expression: Expression,
}

impl PrimitiveAlternative {
    pub fn new(primitive: impl Into<Primitive>, expression: impl Into<Expression>) -> Self {
        Self {
            primitive: primitive.into(),
            expression: expression.into(),
        }
    }

    pub fn primitive(&self) -> Primitive {
        self.primitive
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        self.expression.find_variables()
    }
}
