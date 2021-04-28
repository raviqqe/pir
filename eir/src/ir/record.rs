use super::expression::Expression;
use crate::types;
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    type_: types::Reference,
    elements: Vec<Expression>,
}

impl Record {
    pub fn new(type_: types::Reference, elements: Vec<Expression>) -> Self {
        Self { type_, elements }
    }

    pub fn type_(&self) -> &types::Reference {
        &self.type_
    }

    pub fn elements(&self) -> &[Expression] {
        &self.elements
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        self.elements
            .iter()
            .flat_map(|element| element.find_variables())
            .collect()
    }
}
