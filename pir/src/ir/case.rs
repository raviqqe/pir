use super::{primitive_case::PrimitiveCase, variant_case::VariantCase};

#[derive(Clone, Debug, PartialEq)]
pub enum Case {
    Primitive(PrimitiveCase),
    Variant(VariantCase),
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
