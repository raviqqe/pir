use super::definition::Definition;
use super::expression::Expression;
use std::collections::HashSet;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct LetRecursive {
    definitions: Vec<Definition>,
    expression: Arc<Expression>,
}

impl LetRecursive {
    pub fn new(definitions: Vec<Definition>, expression: impl Into<Expression>) -> Self {
        Self {
            definitions,
            expression: Arc::new(expression.into()),
        }
    }

    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        let mut variables = self.expression.find_variables();

        for definition in &self.definitions {
            variables.extend(definition.find_variables());
        }

        for definition in &self.definitions {
            variables.remove(definition.name());
        }

        variables
    }
}
