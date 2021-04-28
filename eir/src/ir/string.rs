#[derive(Clone, Debug, PartialEq)]
pub struct EirString {
    value: Vec<u8>,
}

impl EirString {
    pub fn new(value: Vec<u8>) -> Self {
        Self { value }
    }

    pub fn value(&self) -> &[u8] {
        &self.value
    }
}
