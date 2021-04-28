use super::expression::Expression;
use crate::types::Type;
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
pub struct VariantAlternative {
    type_: Type,
    name: String,
    expression: Expression,
}

impl VariantAlternative {
    pub fn new(
        type_: impl Into<Type>,
        name: impl Into<String>,
        expression: impl Into<Expression>,
    ) -> Self {
        Self {
            type_: type_.into(),
            name: name.into(),
            expression: expression.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        let mut variables = self.expression.find_variables();

        variables.remove(&self.name);

        variables
    }
}
