use super::comparison_operator::ComparisonOperator;
use super::expression::Expression;
use std::collections::HashSet;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct ComparisonOperation {
    operator: ComparisonOperator,
    lhs: Arc<Expression>,
    rhs: Arc<Expression>,
}

impl ComparisonOperation {
    pub fn new(
        operator: ComparisonOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Self {
        Self {
            operator,
            lhs: Arc::new(lhs.into()),
            rhs: Arc::new(rhs.into()),
        }
    }

    pub fn operator(&self) -> ComparisonOperator {
        self.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        self.lhs
            .find_variables()
            .into_iter()
            .chain(self.rhs.find_variables())
            .collect()
    }
}
