use super::primitive_case::PrimitiveCase;
use super::variant_case::VariantCase;
use std::collections::HashSet;

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
