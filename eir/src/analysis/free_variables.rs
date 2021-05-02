use crate::ir::*;
use std::collections::HashSet;

pub fn find_free_variables(expression: &Expression) -> HashSet<String> {
    find_in_expression(expression)
}

fn find_in_expression(expression: &Expression) -> HashSet<String> {
    match expression {
        Expression::ArithmeticOperation(operation) => find_in_expression(operation.lhs())
            .into_iter()
            .chain(find_in_expression(operation.rhs()))
            .collect(),
        Expression::Case(case) => find_in_case(case),
        Expression::ComparisonOperation(operation) => find_in_expression(operation.lhs())
            .into_iter()
            .chain(find_in_expression(operation.rhs()))
            .collect(),
        Expression::FunctionApplication(application) => find_in_expression(application.function())
            .into_iter()
            .chain(find_in_expression(application.argument()))
            .collect(),
        Expression::LetRecursive(let_) => find_in_expression(let_.expression())
            .into_iter()
            .chain(let_.definitions().iter().flat_map(find_in_definition))
            .filter(|variable| {
                let_.definitions()
                    .iter()
                    .all(|definition| definition.name() != variable)
            })
            .collect(),
        Expression::Let(let_) => find_in_expression(let_.bound_expression())
            .into_iter()
            .chain(
                find_in_expression(let_.expression())
                    .into_iter()
                    .filter(|variable| variable != let_.name()),
            )
            .collect(),
        Expression::Record(record) => record
            .elements()
            .iter()
            .flat_map(find_in_expression)
            .collect(),
        Expression::RecordElement(element) => find_in_expression(element.record()),
        Expression::Variable(variable) => vec![variable.name().into()].into_iter().collect(),
        Expression::Variant(variant) => find_in_expression(variant.payload()),
        Expression::Primitive(_) | Expression::String(_) => HashSet::new(),
    }
}

fn find_in_case(case: &Case) -> HashSet<String> {
    match case {
        Case::Primitive(case) => find_in_expression(case.argument())
            .into_iter()
            .chain(
                case.alternatives()
                    .iter()
                    .flat_map(|alternative| find_in_expression(alternative.expression())),
            )
            .chain(
                case.default_alternative()
                    .into_iter()
                    .flat_map(find_in_expression),
            )
            .collect(),
        Case::Variant(case) => find_in_expression(case.argument())
            .into_iter()
            .chain(case.alternatives().iter().flat_map(|alternative| {
                find_in_expression(alternative.expression())
                    .into_iter()
                    .filter(|variable| variable != alternative.name())
                    .collect::<HashSet<_>>()
            }))
            .chain(
                case.default_alternative()
                    .into_iter()
                    .flat_map(find_in_expression),
            )
            .collect(),
    }
}

fn find_in_definition(definition: &Definition) -> HashSet<String> {
    find_in_expression(definition.body())
        .into_iter()
        .filter(|variable| {
            variable != definition.name()
                && definition
                    .arguments()
                    .iter()
                    .all(|argument| variable != argument.name())
        })
        .collect()
}
