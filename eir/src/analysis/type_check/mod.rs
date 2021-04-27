mod error;

use crate::ir::*;
use crate::types::{self, Type};
pub use error::TypeCheckError;
use std::collections::*;

pub fn check_types(module: &Module) -> Result<(), TypeCheckError> {
    let types = module
        .type_definitions()
        .iter()
        .map(|definition| (definition.name(), definition.type_()))
        .collect();
    let mut variables = HashMap::<&str, Type>::new();

    for declaration in module.foreign_declarations() {
        variables.insert(declaration.name(), declaration.type_().clone().into());
    }

    for declaration in module.declarations() {
        variables.insert(declaration.name(), declaration.type_().clone().into());
    }

    for definition in module.definitions() {
        variables.insert(definition.name(), definition.type_().clone().into());
    }

    for definition in module.definitions() {
        check_definition(definition, &variables, &types)?;
    }

    for definition in module.foreign_definitions() {
        if !variables.contains_key(definition.name()) {
            return Err(TypeCheckError::ForeignDefinitionNotFound(
                definition.clone(),
            ));
        }
    }

    Ok(())
}

fn check_definition(
    definition: &Definition,
    variables: &HashMap<&str, Type>,
    types: &HashMap<&str, &types::Record>,
) -> Result<(), TypeCheckError> {
    let mut variables = variables.clone();

    for argument in definition
        .environment()
        .iter()
        .chain(definition.arguments())
    {
        variables.insert(argument.name(), argument.type_().clone());
    }

    check_equality(
        &check_expression(definition.body(), &variables, types)?,
        &definition.result_type().clone(),
    )
}

fn check_expression(
    expression: &Expression,
    variables: &HashMap<&str, Type>,
    types: &HashMap<&str, &types::Record>,
) -> Result<Type, TypeCheckError> {
    let check_expression = |expression, variables| check_expression(expression, variables, types);

    Ok(match expression {
        Expression::ArithmeticOperation(operation) => {
            let lhs_type = check_expression(operation.lhs(), variables)?;
            let rhs_type = check_expression(operation.rhs(), variables)?;

            if !lhs_type.is_primitive() || !rhs_type.is_primitive() || lhs_type != rhs_type {
                return Err(TypeCheckError::TypesNotMatched(lhs_type, rhs_type));
            }

            lhs_type
        }
        Expression::Case(case) => check_case(case, variables, types)?,
        Expression::ComparisonOperation(operation) => {
            let lhs_type = check_expression(operation.lhs(), variables)?;
            let rhs_type = check_expression(operation.rhs(), variables)?;

            if !lhs_type.is_primitive() || !rhs_type.is_primitive() || lhs_type != rhs_type {
                return Err(TypeCheckError::TypesNotMatched(lhs_type, rhs_type));
            }

            types::Primitive::Boolean.into()
        }
        Expression::FunctionApplication(function_application) => {
            let function_type = check_expression(function_application.function(), variables)?
                .into_function()
                .ok_or_else(|| {
                    TypeCheckError::FunctionExpected(function_application.function().clone())
                })?;

            check_equality(
                &check_expression(function_application.argument(), variables)?,
                function_type.argument(),
            )?;

            function_type.result().clone()
        }
        Expression::LetRecursive(let_recursive) => {
            let mut variables = variables.clone();

            for definition in let_recursive.definitions() {
                variables.insert(definition.name(), definition.type_().clone().into());
            }

            for definition in let_recursive.definitions() {
                check_definition(definition, &variables, &types)?;
            }

            check_expression(let_recursive.expression(), &variables)?
        }
        Expression::Let(let_) => {
            check_equality(
                &check_expression(let_.bound_expression(), variables)?,
                let_.type_(),
            )?;

            let mut variables = variables.clone();
            variables.insert(let_.name(), let_.type_().clone());

            check_expression(let_.expression(), &variables)?
        }
        Expression::Primitive(primitive) => Ok(check_primitive(primitive).into())?,
        Expression::Record(record) => {
            let record_type = types
                .get(record.type_().name())
                .ok_or_else(|| TypeCheckError::TypeNotFound(record.type_().clone()))?;

            if record.elements().len() != record_type.elements().len() {
                return Err(TypeCheckError::WrongElementCount(expression.clone()));
            }

            for (element, element_type) in record.elements().iter().zip(record_type.elements()) {
                check_equality(&check_expression(element, variables)?, &element_type)?;
            }

            record.type_().clone().into()
        }
        Expression::RecordElement(element) => {
            check_equality(
                &check_expression(element.record(), variables)?,
                &element.type_().clone().into(),
            )?;

            types
                .get(element.type_().name())
                .ok_or_else(|| TypeCheckError::TypeNotFound(element.type_().clone()))?
                .elements()
                .get(element.index())
                .ok_or_else(|| TypeCheckError::ElementIndexOutOfBounds(element.clone()))?
                .clone()
        }
        Expression::String(_) => Type::String,
        Expression::Variable(variable) => check_variable(variable, variables)?,
        Expression::Variant(variant) => {
            if matches!(variant.type_(), Type::Variant) {
                return Err(TypeCheckError::VariantInVariant(variant.clone()));
            }

            check_equality(
                &check_expression(variant.payload(), variables)?,
                variant.type_(),
            )?;

            Type::Variant
        }
    })
}

fn check_case(
    case: &Case,
    variables: &HashMap<&str, Type>,
    types: &HashMap<&str, &types::Record>,
) -> Result<Type, TypeCheckError> {
    let check_expression = |expression: &Expression, variables: &HashMap<&str, Type>| {
        check_expression(expression, variables, types)
    };

    match case {
        Case::Primitive(case) => {
            let argument_type = check_expression(case.argument(), variables)?;
            let mut expression_type = None;

            for alternative in case.alternatives() {
                check_equality(
                    &check_primitive(alternative.primitive()).into(),
                    &argument_type.clone(),
                )?;

                let alternative_type = check_expression(alternative.expression(), variables)?;

                if let Some(expression_type) = &expression_type {
                    check_equality(&alternative_type, expression_type)?;
                } else {
                    expression_type = Some(alternative_type);
                }
            }

            if let Some(expression) = case.default_alternative() {
                let alternative_type = check_expression(expression, &variables)?;

                if let Some(expression_type) = &expression_type {
                    check_equality(&alternative_type, expression_type)?;
                } else {
                    expression_type = Some(alternative_type);
                }
            }

            expression_type.ok_or_else(|| TypeCheckError::NoAlternativeFound(case.clone().into()))
        }
        Case::Variant(case) => {
            check_equality(
                &check_expression(case.argument(), variables)?,
                &Type::Variant,
            )?;

            let mut expression_type = None;

            for alternative in case.alternatives() {
                let mut variables = variables.clone();

                variables.insert(alternative.name(), alternative.type_().clone());

                let alternative_type = check_expression(alternative.expression(), &variables)?;

                if let Some(expression_type) = &expression_type {
                    check_equality(&alternative_type, expression_type)?;
                } else {
                    expression_type = Some(alternative_type);
                }
            }

            if let Some(expression) = case.default_alternative() {
                let alternative_type = check_expression(expression, &variables)?;

                if let Some(expression_type) = &expression_type {
                    check_equality(&alternative_type, expression_type)?;
                } else {
                    expression_type = Some(alternative_type);
                }
            }

            expression_type.ok_or_else(|| TypeCheckError::NoAlternativeFound(case.clone().into()))
        }
    }
}

fn check_primitive(primitive: &Primitive) -> types::Primitive {
    match primitive {
        Primitive::Boolean(_) => types::Primitive::Boolean,
        Primitive::Float32(_) => types::Primitive::Float32,
        Primitive::Float64(_) => types::Primitive::Float64,
        Primitive::Integer8(_) => types::Primitive::Integer8,
        Primitive::Integer32(_) => types::Primitive::Integer32,
        Primitive::Integer64(_) => types::Primitive::Integer64,
    }
}

fn check_variable(
    variable: &Variable,
    variables: &HashMap<&str, Type>,
) -> Result<Type, TypeCheckError> {
    variables
        .get(variable.name())
        .cloned()
        .ok_or_else(|| TypeCheckError::VariableNotFound(variable.clone()))
}

fn check_equality(one: &Type, other: &Type) -> Result<(), TypeCheckError> {
    if one == other {
        Ok(())
    } else {
        Err(TypeCheckError::TypesNotMatched(one.clone(), other.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::check_types;
    use super::error::*;
    use crate::ir::*;
    use crate::types::{self, Type};

    fn create_module_from_definitions(definitions: Vec<Definition>) -> Module {
        Module::new(vec![], vec![], vec![], vec![], definitions)
    }

    fn create_module_with_records(
        type_definitions: Vec<TypeDefinition>,
        definitions: Vec<Definition>,
    ) -> Module {
        Module::new(type_definitions, vec![], vec![], vec![], definitions)
    }

    #[test]
    fn check_types_with_empty_modules() {
        assert_eq!(
            check_types(&Module::new(vec![], vec![], vec![], vec![], vec![])),
            Ok(())
        );
    }

    #[test]
    fn check_types_of_variables() {
        let module = create_module_from_definitions(vec![Definition::new(
            "f",
            vec![Argument::new("x", types::Primitive::Float64)],
            Variable::new("x"),
            types::Primitive::Float64,
        )]);
        assert_eq!(check_types(&module), Ok(()));
    }

    #[test]
    fn fail_to_check_types_of_variables() {
        let module = create_module_from_definitions(vec![
            Definition::new(
                "f",
                vec![Argument::new("x", types::Primitive::Float64)],
                42.0,
                types::Primitive::Float64,
            ),
            Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                Variable::new("f"),
                types::Primitive::Float64,
            ),
        ]);

        assert!(matches!(
            check_types(&module),
            Err(TypeCheckError::TypesNotMatched(_, _))
        ));
    }

    #[test]
    fn check_types_of_functions() {
        let module = create_module_from_definitions(vec![Definition::new(
            "f",
            vec![Argument::new("x", types::Primitive::Float64)],
            42.0,
            types::Primitive::Float64,
        )]);

        assert_eq!(check_types(&module), Ok(()));
    }

    #[test]
    fn fail_to_check_types_of_functions() {
        let module = create_module_from_definitions(vec![
            Definition::new(
                "f",
                vec![Argument::new("x", types::Primitive::Float64)],
                42.0,
                types::Primitive::Float64,
            ),
            Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                Variable::new("f"),
                types::Primitive::Float64,
            ),
        ]);

        assert!(matches!(
            check_types(&module),
            Err(TypeCheckError::TypesNotMatched(_, _))
        ));
    }

    #[test]
    fn check_types_of_function_applications() {
        let module = create_module_from_definitions(vec![
            Definition::new(
                "f",
                vec![Argument::new("x", types::Primitive::Float64)],
                42.0,
                types::Primitive::Float64,
            ),
            Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                FunctionApplication::new(Variable::new("f"), 42.0),
                types::Primitive::Float64,
            ),
        ]);

        assert_eq!(check_types(&module), Ok(()));
    }

    #[test]
    fn fail_to_check_types_of_function_applications() {
        let module = create_module_from_definitions(vec![
            Definition::new(
                "f",
                vec![Argument::new("x", types::Primitive::Float64)],
                42.0,
                types::Primitive::Float64,
            ),
            Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                FunctionApplication::new(FunctionApplication::new(Variable::new("f"), 42.0), 42.0),
                types::Primitive::Float64,
            ),
        ]);

        assert!(matches!(
            check_types(&module),
            Err(TypeCheckError::FunctionExpected(_))
        ));
    }

    #[test]
    fn fail_to_check_types_because_of_missing_variables() {
        let module = create_module_from_definitions(vec![Definition::new(
            "f",
            vec![Argument::new("x", types::Primitive::Float64)],
            Variable::new("y"),
            types::Primitive::Float64,
        )]);

        assert!(matches!(
            check_types(&module),
            Err(TypeCheckError::VariableNotFound(_))
        ));
    }

    #[test]
    fn check_types_of_nested_let_expressions() {
        let module = create_module_from_definitions(vec![Definition::new(
            "f",
            vec![Argument::new("x", types::Primitive::Float64)],
            Let::new(
                "y",
                types::Primitive::Float64,
                42.0,
                Let::new(
                    "z",
                    types::Primitive::Float64,
                    Variable::new("y"),
                    Variable::new("z"),
                ),
            ),
            types::Primitive::Float64,
        )]);

        assert_eq!(check_types(&module), Ok(()));
    }

    #[test]
    fn fail_to_check_types_of_let_expression() {
        let module = create_module_from_definitions(vec![
            Definition::new(
                "f",
                vec![Argument::new("x", types::Primitive::Float64)],
                42.0,
                types::Primitive::Float64,
            ),
            Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                Let::new(
                    "y",
                    types::Primitive::Float64,
                    Variable::new("f"),
                    Variable::new("y"),
                ),
                types::Primitive::Float64,
            ),
        ]);

        assert!(matches!(
            check_types(&module),
            Err(TypeCheckError::TypesNotMatched(_, _))
        ));
    }

    #[test]
    fn check_types_of_declarations() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![Declaration::new(
                "f",
                types::Function::new(types::Primitive::Float64, types::Primitive::Float64),
            )],
            vec![Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                FunctionApplication::new(Variable::new("f"), Variable::new("x")),
                types::Primitive::Float64,
            )],
        );
        assert_eq!(check_types(&module), Ok(()));
    }

    #[test]
    fn fail_to_check_types_of_declarations() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![Declaration::new(
                "f",
                types::Function::new(types::Primitive::Float64, types::Primitive::Float64),
            )],
            vec![Definition::new(
                "g",
                vec![Argument::new("x", types::Primitive::Float64)],
                Variable::new("f"),
                types::Primitive::Float64,
            )],
        );

        assert!(matches!(
            check_types(&module),
            Err(TypeCheckError::TypesNotMatched(_, _))
        ));
    }

    mod case_expressions {
        use super::*;

        mod variant {
            use super::*;

            #[test]
            fn check_case_expressions_only_with_default_alternative() {
                assert_eq!(
                    check_types(&create_module_from_definitions(vec![Definition::new(
                        "f",
                        vec![Argument::new("x", Type::Variant)],
                        VariantCase::new(Variable::new("x"), vec![], Some(42.0.into()),),
                        types::Primitive::Float64,
                    )])),
                    Ok(())
                );
            }

            #[test]
            fn check_case_expressions_with_one_alternative() {
                assert_eq!(
                    check_types(&create_module_from_definitions(vec![Definition::new(
                        "f",
                        vec![Argument::new("x", Type::Variant)],
                        VariantCase::new(
                            Variable::new("x"),
                            vec![VariantAlternative::new(
                                types::Primitive::Float64,
                                "y",
                                Variable::new("y")
                            )],
                            None
                        ),
                        types::Primitive::Float64,
                    )])),
                    Ok(())
                );
            }

            #[test]
            fn fail_to_check_case_expressions_without_alternatives() {
                let module = create_module_from_definitions(vec![Definition::new(
                    "f",
                    vec![Argument::new("x", Type::Variant)],
                    VariantCase::new(Variable::new("x"), vec![], None),
                    types::Primitive::Float64,
                )]);

                assert!(matches!(
                    check_types(&module),
                    Err(TypeCheckError::NoAlternativeFound(_))
                ));
            }

            #[test]
            fn fail_to_check_case_expressions_with_inconsistent_alternative_types() {
                let module = create_module_from_definitions(vec![Definition::with_environment(
                    "f",
                    vec![],
                    vec![Argument::new("x", Type::Variant)],
                    VariantCase::new(
                        Variable::new("x"),
                        vec![
                            VariantAlternative::new(
                                types::Primitive::Integer64,
                                "x",
                                Variable::new("x"),
                            ),
                            VariantAlternative::new(types::Primitive::Float64, "x", 42.0),
                        ],
                        None,
                    ),
                    types::Primitive::Float64,
                )]);

                assert!(matches!(
                    check_types(&module),
                    Err(TypeCheckError::TypesNotMatched(_, _))
                ));
            }

            #[test]
            fn fail_for_unmatched_case_type() {
                assert!(matches!(
                    check_types(&create_module_from_definitions(vec![
                        Definition::with_environment(
                            "f",
                            vec![],
                            vec![Argument::new("x", Type::Variant)],
                            VariantCase::new(
                                Variable::new("x"),
                                vec![VariantAlternative::new(
                                    types::Reference::new("foo"),
                                    "y",
                                    Variable::new("y")
                                )],
                                None
                            ),
                            types::Reference::new("bar"),
                        )
                    ])),
                    Err(TypeCheckError::TypesNotMatched(_, _))
                ));
            }
        }

        mod primitive {
            use super::*;

            #[test]
            fn check_case_expressions_only_with_default_alternative() {
                assert_eq!(
                    check_types(&create_module_from_definitions(vec![
                        Definition::with_environment(
                            "f",
                            vec![],
                            vec![Argument::new("x", types::Primitive::Float64)],
                            PrimitiveCase::new(42.0, vec![], Some(42.0.into()),),
                            types::Primitive::Float64,
                        )
                    ])),
                    Ok(())
                );
            }

            #[test]
            fn check_case_expressions_with_one_alternative() {
                assert_eq!(
                    check_types(&create_module_from_definitions(vec![
                        Definition::with_environment(
                            "f",
                            vec![],
                            vec![Argument::new("x", types::Primitive::Float64)],
                            PrimitiveCase::new(
                                42.0,
                                vec![PrimitiveAlternative::new(42.0, 42.0)],
                                None
                            ),
                            types::Primitive::Float64,
                        )
                    ],)),
                    Ok(())
                );
            }

            #[test]
            fn check_case_expressions_with_one_alternative_and_default_alternative() {
                assert_eq!(
                    check_types(&create_module_from_definitions(vec![
                        Definition::with_environment(
                            "f",
                            vec![],
                            vec![Argument::new("x", types::Primitive::Float64)],
                            PrimitiveCase::new(
                                42.0,
                                vec![PrimitiveAlternative::new(42.0, 42.0)],
                                Some(42.0.into())
                            ),
                            types::Primitive::Float64,
                        )
                    ],)),
                    Ok(())
                );
            }

            #[test]
            fn fail_for_unmatched_case_type() {
                assert!(matches!(
                    check_types(&create_module_from_definitions(vec![
                        Definition::with_environment(
                            "f",
                            vec![],
                            vec![Argument::new("x", types::Primitive::Float64)],
                            PrimitiveCase::new(
                                42.0,
                                vec![PrimitiveAlternative::new(42, 42.0)],
                                Some(42.0.into())
                            ),
                            types::Primitive::Float64,
                        )
                    ],)),
                    Err(TypeCheckError::TypesNotMatched(_, _))
                ));
            }
        }
    }

    mod records {
        use super::*;

        #[test]
        fn check_records_with_no_element() {
            let reference_type = types::Reference::new("foo");

            assert_eq!(
                check_types(&create_module_with_records(
                    vec![TypeDefinition::new("foo", types::Record::new(vec![]))],
                    vec![Definition::with_environment(
                        "f",
                        vec![],
                        vec![Argument::new("x", types::Primitive::Float64)],
                        Record::new(reference_type.clone(), vec![]),
                        reference_type,
                    )],
                )),
                Ok(())
            );
        }

        #[test]
        fn check_records_with_elements() {
            let reference_type = types::Reference::new("foo");

            assert_eq!(
                check_types(&create_module_with_records(
                    vec![TypeDefinition::new(
                        "foo",
                        types::Record::new(vec![types::Primitive::Float64.into()])
                    )],
                    vec![Definition::with_environment(
                        "f",
                        vec![],
                        vec![Argument::new("x", types::Primitive::Float64)],
                        Record::new(reference_type.clone(), vec![42.0.into()],),
                        reference_type,
                    )],
                )),
                Ok(())
            );
        }

        #[test]
        fn fail_to_check_records_with_wrong_number_of_elements() {
            let reference_type = types::Reference::new("foo");

            let module = create_module_with_records(
                vec![TypeDefinition::new(
                    "foo",
                    types::Record::new(vec![types::Primitive::Float64.into()]),
                )],
                vec![Definition::with_environment(
                    "f",
                    vec![],
                    vec![Argument::new("x", types::Primitive::Float64)],
                    Record::new(reference_type.clone(), vec![42.0.into(), 42.0.into()]),
                    reference_type,
                )],
            );

            assert!(matches!(
                check_types(&module),
                Err(TypeCheckError::WrongElementCount(_))
            ));
        }

        #[test]
        fn fail_to_check_records_with_wrong_element_type() {
            let reference_type = types::Reference::new("foo");

            let module = create_module_with_records(
                vec![TypeDefinition::new(
                    "foo",
                    types::Record::new(vec![types::Primitive::Float64.into()]),
                )],
                vec![Definition::with_environment(
                    "f",
                    vec![],
                    vec![Argument::new("x", types::Primitive::Float64)],
                    Record::new(reference_type.clone(), vec![true.into()]),
                    reference_type,
                )],
            );

            assert!(matches!(
                check_types(&module),
                Err(TypeCheckError::TypesNotMatched(_, _))
            ));
        }

        #[test]
        fn check_record_element() {
            let reference_type = types::Reference::new("foo");

            assert_eq!(
                check_types(&create_module_with_records(
                    vec![TypeDefinition::new(
                        "foo",
                        types::Record::new(vec![types::Primitive::Float64.into()])
                    )],
                    vec![Definition::with_environment(
                        "f",
                        vec![],
                        vec![Argument::new("x", types::Primitive::Float64)],
                        RecordElement::new(
                            reference_type.clone(),
                            0,
                            Record::new(reference_type, vec![42.0.into()],)
                        ),
                        types::Primitive::Float64
                    )],
                )),
                Ok(())
            );
        }
    }

    mod variants {
        use super::*;

        #[test]
        fn check_variant() {
            assert_eq!(
                check_types(&create_module_from_definitions(vec![
                    Definition::with_environment(
                        "f",
                        vec![],
                        vec![Argument::new("x", types::Primitive::Float64)],
                        Variant::new(types::Primitive::Float64, Primitive::Float64(42.0),),
                        Type::Variant
                    )
                ],)),
                Ok(())
            );
        }

        #[test]
        fn fail_to_check_variant_in_variant() {
            assert!(matches!(
                check_types(&create_module_from_definitions(vec![
                    Definition::with_environment(
                        "f",
                        vec![],
                        vec![Argument::new("x", Type::Variant)],
                        Variant::new(Type::Variant, Variable::new("x")),
                        Type::Variant
                    )
                ],)),
                Err(TypeCheckError::VariantInVariant(_))
            ));
        }
    }

    #[test]
    fn check_add_operator() {
        let module = create_module_from_definitions(vec![Definition::with_environment(
            "f",
            vec![],
            vec![Argument::new("x", types::Primitive::Float64)],
            ArithmeticOperation::new(ArithmeticOperator::Add, 42.0, 42.0),
            types::Primitive::Float64,
        )]);
        assert_eq!(check_types(&module), Ok(()));
    }

    #[test]
    fn check_equality_operator() {
        let module = create_module_from_definitions(vec![Definition::with_environment(
            "f",
            vec![],
            vec![Argument::new("x", types::Primitive::Float64)],
            ComparisonOperation::new(ComparisonOperator::Equal, 42.0, 42.0),
            types::Primitive::Boolean,
        )]);
        assert_eq!(check_types(&module), Ok(()));
    }

    mod foreign_declarations {
        use super::*;

        #[test]
        fn check_types_of_foreign_declarations() {
            let module = Module::new(
                vec![],
                vec![ForeignDeclaration::new(
                    "f",
                    "g",
                    types::Function::new(types::Primitive::Float64, types::Primitive::Float64),
                    CallingConvention::Target,
                )],
                vec![],
                vec![],
                vec![Definition::new(
                    "g",
                    vec![Argument::new("x", types::Primitive::Float64)],
                    FunctionApplication::new(Variable::new("f"), Variable::new("x")),
                    types::Primitive::Float64,
                )],
            );
            assert_eq!(check_types(&module), Ok(()));
        }

        #[test]
        fn fail_to_check_types_of_foreign_declarations() {
            let module = Module::new(
                vec![],
                vec![ForeignDeclaration::new(
                    "f",
                    "g",
                    types::Function::new(types::Primitive::Float64, types::Primitive::Float64),
                    CallingConvention::Target,
                )],
                vec![],
                vec![],
                vec![Definition::new(
                    "g",
                    vec![Argument::new("x", types::Primitive::Float64)],
                    Variable::new("f"),
                    types::Primitive::Float64,
                )],
            );

            assert!(matches!(
                check_types(&module),
                Err(TypeCheckError::TypesNotMatched(_, _))
            ));
        }
    }

    mod foreign_definitions {
        use super::*;

        #[test]
        fn check_types_of_foreign_definition_for_declaration() {
            let module = Module::new(
                vec![],
                vec![],
                vec![ForeignDefinition::new("f", "g")],
                vec![Declaration::new(
                    "f",
                    types::Function::new(types::Primitive::Float64, types::Primitive::Float64),
                )],
                vec![],
            );

            assert_eq!(check_types(&module), Ok(()));
        }

        #[test]
        fn check_types_of_foreign_definition_for_definition() {
            let module = Module::new(
                vec![],
                vec![],
                vec![ForeignDefinition::new("f", "g")],
                vec![],
                vec![Definition::new(
                    "f",
                    vec![Argument::new("x", types::Primitive::Float64)],
                    Variable::new("x"),
                    types::Primitive::Float64,
                )],
            );

            assert_eq!(check_types(&module), Ok(()));
        }
    }
}
