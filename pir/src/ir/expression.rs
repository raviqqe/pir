use super::{
    arithmetic_operation::ArithmeticOperation, case::Case,
    comparison_operation::ComparisonOperation, function_application::FunctionApplication,
    let_::Let, let_recursive::LetRecursive, primitive::Primitive, primitive_case::PrimitiveCase,
    record::Record, record_element::RecordElement, string::PirString, variable::Variable,
    variant::Variant, variant_case::VariantCase,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    ArithmeticOperation(ArithmeticOperation),
    Case(Case),
    ComparisonOperation(ComparisonOperation),
    FunctionApplication(FunctionApplication),
    Let(Let),
    LetRecursive(LetRecursive),
    Primitive(Primitive),
    Record(Record),
    RecordElement(RecordElement),
    String(PirString),
    Variable(Variable),
    Variant(Variant),
}

impl Expression {
    pub fn to_variable(&self) -> Option<&Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
        }
    }
}

impl From<ArithmeticOperation> for Expression {
    fn from(operation: ArithmeticOperation) -> Self {
        Self::ArithmeticOperation(operation)
    }
}

impl From<Case> for Expression {
    fn from(case: Case) -> Self {
        Self::Case(case)
    }
}

impl From<ComparisonOperation> for Expression {
    fn from(operation: ComparisonOperation) -> Self {
        Self::ComparisonOperation(operation)
    }
}

impl From<PirString> for Expression {
    fn from(string: PirString) -> Self {
        Self::String(string)
    }
}

impl From<FunctionApplication> for Expression {
    fn from(function_application: FunctionApplication) -> Self {
        Self::FunctionApplication(function_application)
    }
}

impl From<LetRecursive> for Expression {
    fn from(let_recursive: LetRecursive) -> Self {
        Self::LetRecursive(let_recursive)
    }
}

impl From<Let> for Expression {
    fn from(let_: Let) -> Self {
        Self::Let(let_)
    }
}

impl<T: Into<Primitive>> From<T> for Expression {
    fn from(primitive: T) -> Self {
        Self::Primitive(primitive.into())
    }
}

impl From<PrimitiveCase> for Expression {
    fn from(primitive_case: PrimitiveCase) -> Self {
        Self::Case(primitive_case.into())
    }
}

impl From<Record> for Expression {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}

impl From<RecordElement> for Expression {
    fn from(element: RecordElement) -> Self {
        Self::RecordElement(element)
    }
}

impl From<Variable> for Expression {
    fn from(variable: Variable) -> Self {
        Self::Variable(variable)
    }
}

impl From<Variant> for Expression {
    fn from(variant: Variant) -> Self {
        Self::Variant(variant)
    }
}

impl From<VariantCase> for Expression {
    fn from(variant_case: VariantCase) -> Self {
        Self::Case(variant_case.into())
    }
}
