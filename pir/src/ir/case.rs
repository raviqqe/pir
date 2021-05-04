use super::{expression::Expression, variant_alternative::VariantAlternative};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    argument: Arc<Expression>,
    alternatives: Vec<VariantAlternative>,
    default_alternative: Option<Arc<Expression>>,
}

impl Case {
    pub fn new(
        argument: impl Into<Expression>,
        alternatives: Vec<VariantAlternative>,
        default_alternative: Option<Expression>,
    ) -> Self {
        Self {
            argument: Arc::new(argument.into()),
            alternatives,
            default_alternative: default_alternative.map(|expression| expression.into()),
        }
    }

    pub fn argument(&self) -> &Expression {
        &self.argument
    }

    pub fn alternatives(&self) -> &[VariantAlternative] {
        &self.alternatives
    }

    pub fn default_alternative(&self) -> Option<&Expression> {
        self.default_alternative
            .as_ref()
            .map(|expression| expression.as_ref())
    }
}
