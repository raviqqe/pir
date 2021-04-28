#[derive(Clone, Debug, PartialEq)]
pub struct EirString {
    value: Vec<u8>,
}

impl EirString {
    pub fn new(value: impl Into<Vec<u8>>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn value(&self) -> &[u8] {
        &self.value
    }
}
