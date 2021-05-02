use super::{expression::Expression, primitive_alternative::PrimitiveAlternative};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct PrimitiveCase {
    argument: Arc<Expression>,
    alternatives: Vec<PrimitiveAlternative>,
    default_alternative: Option<Arc<Expression>>,
}

impl PrimitiveCase {
    pub fn new(
        argument: impl Into<Expression>,
        alternatives: Vec<PrimitiveAlternative>,
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

    pub fn alternatives(&self) -> &[PrimitiveAlternative] {
        &self.alternatives
    }

    pub fn default_alternative(&self) -> Option<&Expression> {
        self.default_alternative
            .as_ref()
            .map(|expression| expression.as_ref())
    }
}
