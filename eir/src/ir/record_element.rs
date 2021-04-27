use super::expression::Expression;
use crate::types::{self, Type};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct RecordElement {
    type_: types::Reference,
    index: usize,
    record: Arc<Expression>,
}

impl RecordElement {
    pub fn new(type_: types::Reference, index: usize, record: impl Into<Expression>) -> Self {
        Self {
            type_,
            index,
            record: record.into().into(),
        }
    }

    pub fn type_(&self) -> &types::Reference {
        &self.type_
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn record(&self) -> &Expression {
        &self.record
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        self.record.find_variables()
    }

    pub(crate) fn infer_environment(&self, variables: &HashMap<String, Type>) -> Self {
        Self {
            type_: self.type_.clone(),
            index: self.index,
            record: self.record.infer_environment(variables).into(),
        }
    }
}
