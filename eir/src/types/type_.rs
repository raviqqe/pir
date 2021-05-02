use super::{function::Function, primitive::Primitive, record::Record};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Function(Function),
    Primitive(Primitive),
    Record(Record),
    String,
    Variant,
}

impl Type {
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }

    pub fn into_function(self) -> Option<Function> {
        match self {
            Self::Function(function) => Some(function),
            _ => None,
        }
    }

    pub fn into_primitive(self) -> Option<Primitive> {
        match self {
            Self::Primitive(primitive) => Some(primitive),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Record> {
        match self {
            Self::Record(record) => Some(record),
            _ => None,
        }
    }
}

impl From<Function> for Type {
    fn from(function: Function) -> Self {
        Self::Function(function)
    }
}

impl From<Primitive> for Type {
    fn from(primitive: Primitive) -> Self {
        Self::Primitive(primitive)
    }
}

impl From<Record> for Type {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}
