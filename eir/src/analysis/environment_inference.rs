use crate::ir::*;
use crate::types::Type;
use std::collections::HashMap;

pub fn infer_environment(module: &Module) -> Module {
    Module::new(
        module.type_definitions().to_vec(),
        module.foreign_declarations().to_vec(),
        module.foreign_definitions().to_vec(),
        module.declarations().to_vec(),
        module
            .definitions()
            .iter()
            .map(|definition| infer_in_definition(definition, &Default::default()))
            .collect(),
    )
}

fn infer_in_definition(definition: &Definition, variables: &HashMap<String, Type>) -> Definition {
    Definition::with_options(
        definition.name(),
        definition
            .body()
            .find_variables()
            .iter()
            .filter_map(|name| {
                variables
                    .get(name)
                    .map(|type_| Argument::new(name, type_.clone()))
            })
            .collect(),
        definition.arguments().to_vec(),
        // Do not include this function itself in variables as it can be global.
        infer_in_expression(
            definition.body(),
            &variables
                .clone()
                .drain()
                .chain(
                    definition
                        .arguments()
                        .iter()
                        .map(|argument| (argument.name().into(), argument.type_().clone())),
                )
                .collect(),
        ),
        definition.result_type().clone(),
        definition.is_thunk(),
    )
}

fn infer_in_expression(expression: &Expression, variables: &HashMap<String, Type>) -> Expression {
    match expression {
        Expression::ArithmeticOperation(operation) => {
            infer_in_arithmetic_operation(operation, variables).into()
        }
        Expression::Case(case) => infer_in_case(case, variables).into(),
        Expression::ComparisonOperation(operation) => {
            infer_in_comparison_operation(operation, variables).into()
        }
        Expression::FunctionApplication(application) => {
            infer_in_function_application(application, variables).into()
        }
        Expression::Let(let_) => infer_in_let(let_, variables).into(),
        Expression::LetRecursive(let_) => infer_in_let_recursive(let_, variables).into(),
        Expression::Record(record) => infer_in_record(record, variables).into(),
        Expression::RecordElement(element) => infer_in_record_element(element, variables).into(),
        Expression::Variant(variant) => infer_in_variant(variant, variables).into(),
        Expression::Primitive(_) | Expression::String(_) | Expression::Variable(_) => {
            expression.clone()
        }
    }
}

fn infer_in_arithmetic_operation(
    operation: &ArithmeticOperation,
    variables: &HashMap<String, Type>,
) -> ArithmeticOperation {
    ArithmeticOperation::new(
        operation.operator(),
        infer_in_expression(operation.lhs(), variables),
        infer_in_expression(operation.rhs(), variables),
    )
}

fn infer_in_case(case: &Case, variables: &HashMap<String, Type>) -> Case {
    match case {
        Case::Primitive(case) => infer_in_primitive_case(case, variables).into(),
        Case::Variant(case) => infer_in_variant_case(case, variables).into(),
    }
}

fn infer_in_primitive_case(
    case: &PrimitiveCase,
    variables: &HashMap<String, Type>,
) -> PrimitiveCase {
    PrimitiveCase::new(
        infer_in_expression(case.argument(), variables),
        case.alternatives()
            .iter()
            .map(|alternative| infer_in_primitive_alternative(alternative, variables))
            .collect(),
        case.default_alternative()
            .map(|expression| infer_in_expression(expression, variables)),
    )
}

fn infer_in_primitive_alternative(
    alternative: &PrimitiveAlternative,
    variables: &HashMap<String, Type>,
) -> PrimitiveAlternative {
    PrimitiveAlternative::new(
        alternative.primitive(),
        infer_in_expression(alternative.expression(), &&variables),
    )
}

fn infer_in_variant_case(case: &VariantCase, variables: &HashMap<String, Type>) -> VariantCase {
    VariantCase::new(
        infer_in_expression(case.argument(), variables),
        case.alternatives()
            .iter()
            .map(|alternative| infer_in_variant_alternative(alternative, variables))
            .collect(),
        case.default_alternative()
            .map(|expression| infer_in_expression(expression, variables)),
    )
}

fn infer_in_variant_alternative(
    alternative: &VariantAlternative,
    variables: &HashMap<String, Type>,
) -> VariantAlternative {
    let mut variables = variables.clone();

    variables.insert(alternative.name().into(), alternative.type_().clone());

    VariantAlternative::new(
        alternative.type_().clone(),
        alternative.name(),
        infer_in_expression(alternative.expression(), &&variables),
    )
}

fn infer_in_comparison_operation(
    operation: &ComparisonOperation,
    variables: &HashMap<String, Type>,
) -> ComparisonOperation {
    ComparisonOperation::new(
        operation.operator(),
        infer_in_expression(operation.lhs(), variables),
        infer_in_expression(operation.rhs(), variables),
    )
}

fn infer_in_function_application(
    application: &FunctionApplication,
    variables: &HashMap<String, Type>,
) -> FunctionApplication {
    FunctionApplication::new(
        infer_in_expression(application.function(), variables),
        infer_in_expression(application.argument(), variables),
    )
}

fn infer_in_let(let_: &Let, variables: &HashMap<String, Type>) -> Let {
    Let::new(
        let_.name(),
        let_.type_().clone(),
        infer_in_expression(let_.bound_expression(), variables),
        infer_in_expression(
            let_.expression(),
            &variables
                .clone()
                .drain()
                .chain(vec![(let_.name().into(), let_.type_().clone())])
                .collect(),
        ),
    )
}

fn infer_in_let_recursive(let_: &LetRecursive, variables: &HashMap<String, Type>) -> LetRecursive {
    let variables = variables
        .clone()
        .drain()
        .chain(
            let_.definitions()
                .iter()
                .map(|definition| (definition.name().into(), definition.type_().clone().into())),
        )
        .collect();

    LetRecursive::new(
        let_.definitions()
            .iter()
            .map(|definition| infer_in_definition(definition, &variables))
            .collect(),
        infer_in_expression(let_.expression(), &variables),
    )
}

fn infer_in_record(record: &Record, variables: &HashMap<String, Type>) -> Record {
    Record::new(
        record.type_().clone(),
        record
            .elements()
            .iter()
            .map(|element| infer_in_expression(element, variables))
            .collect(),
    )
}

fn infer_in_record_element(
    element: &RecordElement,
    variables: &HashMap<String, Type>,
) -> RecordElement {
    RecordElement::new(
        element.type_().clone(),
        element.index(),
        infer_in_expression(element.record(), variables),
    )
}

fn infer_in_variant(variant: &Variant, variables: &HashMap<String, Type>) -> Variant {
    Variant::new(
        variant.type_().clone(),
        infer_in_expression(variant.payload(), variables),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn infer_empty_environment() {
        assert_eq!(
            infer_in_definition(
                &Definition::new(
                    "f",
                    vec![Argument::new("x", types::Primitive::Number)],
                    42.0,
                    types::Primitive::Number
                ),
                &Default::default()
            ),
            Definition::with_environment(
                "f",
                vec![],
                vec![Argument::new("x", types::Primitive::Number)],
                42.0,
                types::Primitive::Number
            )
        );
    }

    #[test]
    fn infer_environment() {
        assert_eq!(
            infer_in_definition(
                &Definition::new(
                    "f",
                    vec![Argument::new("x", types::Primitive::Number)],
                    Variable::new("y"),
                    types::Primitive::Number
                ),
                &vec![("y".into(), types::Primitive::Number.into())]
                    .drain(..)
                    .collect()
            ),
            Definition::with_environment(
                "f",
                vec![Argument::new("y", types::Primitive::Number)],
                vec![Argument::new("x", types::Primitive::Number)],
                Variable::new("y"),
                types::Primitive::Number
            )
        );
    }

    #[test]
    fn infer_environment_idempotently() {
        let variables = vec![("y".into(), types::Primitive::Number.into())]
            .drain(..)
            .collect();

        assert_eq!(
            infer_in_definition(
                &infer_in_definition(
                    &Definition::new(
                        "f",
                        vec![Argument::new("x", types::Primitive::Number)],
                        Variable::new("y"),
                        types::Primitive::Number
                    ),
                    &variables
                ),
                &variables
            ),
            Definition::with_environment(
                "f",
                vec![Argument::new("y", types::Primitive::Number)],
                vec![Argument::new("x", types::Primitive::Number)],
                Variable::new("y"),
                types::Primitive::Number
            )
        );
    }
}
