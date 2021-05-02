use crate::{ir::*, types::Type};
use std::collections::*;

pub fn collect_variant_types(module: &Module) -> HashSet<Type> {
    module
        .definitions()
        .iter()
        .flat_map(|definition| collect_from_definition(definition))
        .collect()
}

fn collect_from_definition(definition: &Definition) -> HashSet<Type> {
    collect_from_expression(definition.body())
}

fn collect_from_expression(expression: &Expression) -> HashSet<Type> {
    match expression {
        Expression::ArithmeticOperation(operation) => collect_from_expression(operation.lhs())
            .drain()
            .chain(collect_from_expression(operation.rhs()))
            .collect(),
        Expression::Case(case) => collect_from_case(case),
        Expression::ComparisonOperation(operation) => collect_from_expression(operation.lhs())
            .drain()
            .chain(collect_from_expression(operation.rhs()))
            .collect(),
        Expression::FunctionApplication(application) => {
            collect_from_expression(application.function())
                .drain()
                .chain(collect_from_expression(application.argument()))
                .collect()
        }
        Expression::Let(let_) => collect_from_expression(let_.bound_expression())
            .drain()
            .chain(collect_from_expression(let_.expression()))
            .collect(),
        Expression::LetRecursive(let_) => let_
            .definitions()
            .iter()
            .flat_map(collect_from_definition)
            .chain(collect_from_expression(let_.expression()))
            .collect(),
        Expression::Record(record) => record
            .elements()
            .iter()
            .flat_map(collect_from_expression)
            .collect(),
        Expression::RecordElement(element) => collect_from_expression(element.record()),
        Expression::Variant(variant) => vec![variant.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(variant.payload()))
            .collect(),
        Expression::Primitive(_) | Expression::String(_) | Expression::Variable(_) => {
            Default::default()
        }
    }
}

fn collect_from_case(case: &Case) -> HashSet<Type> {
    match case {
        Case::Primitive(case) => case
            .alternatives()
            .iter()
            .flat_map(|alternative| collect_from_expression(alternative.expression()))
            .chain(
                case.default_alternative()
                    .map(collect_from_expression)
                    .unwrap_or_default(),
            )
            .collect(),
        Case::Variant(case) => case
            .alternatives()
            .iter()
            .flat_map(|alternative| {
                vec![alternative.type_().clone()]
                    .into_iter()
                    .chain(collect_from_expression(alternative.expression()))
            })
            .chain(
                case.default_alternative()
                    .map(collect_from_expression)
                    .unwrap_or_default(),
            )
            .collect(),
    }
}
