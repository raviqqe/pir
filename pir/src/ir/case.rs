use super::variant_case::VariantCase;

#[derive(Clone, Debug, PartialEq)]
pub enum Case {
    Variant(VariantCase),
}

impl From<VariantCase> for Case {
    fn from(variant_case: VariantCase) -> Self {
        Self::Variant(variant_case)
    }
}
