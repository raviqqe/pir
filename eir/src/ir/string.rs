#[derive(Clone, Debug, PartialEq)]
pub struct EirString {
    value: String,
}

impl EirString {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}
