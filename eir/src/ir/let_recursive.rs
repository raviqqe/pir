use super::{definition::Definition, expression::Expression};
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
}
