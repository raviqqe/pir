#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Primitive {
    Boolean(bool),
    Number(f64),
}

impl From<bool> for Primitive {
    fn from(boolean: bool) -> Self {
        Self::Boolean(boolean)
    }
}

impl From<f64> for Primitive {
    fn from(number: f64) -> Self {
        Self::Number(number)
    }
}
