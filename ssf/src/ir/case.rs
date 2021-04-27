use super::primitive_case::PrimitiveCase;
use super::variant_case::VariantCase;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq)]
pub enum Case {
    Primitive(PrimitiveCase),
    Variant(VariantCase),
}

impl Case {
    pub(crate) fn find_variables(&self) -> HashSet<String> {
        match self {
            Self::Primitive(primitive_case) => primitive_case.find_variables(),
            Self::Variant(variant_case) => variant_case.find_variables(),
        }
    }

    pub(crate) fn infer_environment(&self, variables: &HashMap<String, Type>) -> Self {
        match self {
            Self::Primitive(primitive_case) => primitive_case.infer_environment(variables).into(),
            Self::Variant(variant_case) => variant_case.infer_environment(variables).into(),
        }
    }
}

impl From<PrimitiveCase> for Case {
    fn from(primitive_case: PrimitiveCase) -> Self {
        Self::Primitive(primitive_case)
    }
}

impl From<VariantCase> for Case {
    fn from(variant_case: VariantCase) -> Self {
        Self::Variant(variant_case)
    }
}
