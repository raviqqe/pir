mod closures;
mod declarations;
mod definitions;
mod entry_functions;
mod error;
mod expressions;
mod foreign_declarations;
mod foreign_definitions;
mod function_applications;
mod type_information;
mod types;

use declarations::compile_declaration;
use definitions::compile_definition;
pub use error::CompileError;
use foreign_declarations::compile_foreign_declaration;
use foreign_definitions::compile_foreign_definition;
use std::collections::HashMap;
use type_information::compile_type_information_global_variable;

pub fn compile(module: &eir::ir::Module) -> Result<fmm::ir::Module, CompileError> {
    eir::analysis::check_types(module)?;

    let module_builder = fmm::build::ModuleBuilder::new();
    let types = module
        .type_definitions()
        .iter()
        .map(|definition| (definition.name().into(), definition.type_().clone()))
        .collect();

    for type_ in &eir::analysis::collect_variant_types(module) {
        compile_type_information_global_variable(&module_builder, type_)?;
    }

    for declaration in module.foreign_declarations() {
        compile_foreign_declaration(&module_builder, declaration, &types)?;
    }

    for declaration in module.declarations() {
        compile_declaration(&module_builder, declaration, &types);
    }

    let global_variables = compile_global_variables(module, &types);

    for definition in module.definitions() {
        compile_definition(&module_builder, definition, &global_variables, &types)?;
    }

    let function_types = module
        .foreign_declarations()
        .iter()
        .map(|declaration| (declaration.name(), declaration.type_()))
        .chain(
            module
                .declarations()
                .iter()
                .map(|declaration| (declaration.name(), declaration.type_())),
        )
        .chain(
            module
                .definitions()
                .iter()
                .map(|definition| (definition.name(), definition.type_())),
        )
        .collect::<HashMap<_, _>>();

    for definition in module.foreign_definitions() {
        compile_foreign_definition(
            &module_builder,
            definition,
            function_types[definition.name()],
            &global_variables[definition.name()],
            &types,
        )?;
    }

    Ok(module_builder.as_module())
}

fn compile_global_variables(
    module: &eir::ir::Module,
    types: &HashMap<String, eir::types::Record>,
) -> HashMap<String, fmm::build::TypedExpression> {
    module
        .foreign_declarations()
        .iter()
        .map(|declaration| {
            (
                declaration.name().into(),
                fmm::build::variable(
                    declaration.name(),
                    fmm::types::Pointer::new(types::compile_unsized_closure(
                        declaration.type_(),
                        types,
                    )),
                ),
            )
        })
        .chain(module.declarations().iter().map(|declaration| {
            (
                declaration.name().into(),
                fmm::build::variable(
                    declaration.name(),
                    fmm::types::Pointer::new(types::compile_unsized_closure(
                        declaration.type_(),
                        &types,
                    )),
                ),
            )
        }))
        .chain(module.definitions().iter().map(|definition| {
            (
                definition.name().into(),
                fmm::build::bit_cast(
                    fmm::types::Pointer::new(types::compile_unsized_closure(
                        definition.type_(),
                        &types,
                    )),
                    fmm::build::variable(
                        definition.name(),
                        fmm::types::Pointer::new(types::compile_sized_closure(definition, types)),
                    ),
                )
                .into(),
            )
        }))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile_module(module: &eir::ir::Module) {
        let module = compile(module).unwrap();

        compile_final_module(&module);
        compile_final_module(
            &fmm::analysis::transform_to_cps(&module, fmm::types::Record::new(vec![])).unwrap(),
        );
    }

    fn compile_final_module(module: &fmm::ir::Module) {
        fmm::analysis::check_types(module).unwrap();

        fmm_llvm::compile_to_object(
            &module,
            &fmm_llvm::HeapConfiguration {
                allocate_function_name: "allocate_heap".into(),
                reallocate_function_name: "reallocate_heap".into(),
                free_function_name: "free_heap".into(),
            },
            None,
        )
        .unwrap();
    }

    fn create_module_with_definitions(definitions: Vec<eir::ir::Definition>) -> eir::ir::Module {
        eir::ir::Module::new(vec![], vec![], vec![], vec![], definitions)
    }

    fn create_module_with_type_definitions(
        variant_definitions: Vec<eir::ir::TypeDefinition>,
        definitions: Vec<eir::ir::Definition>,
    ) -> eir::ir::Module {
        eir::ir::Module::new(variant_definitions, vec![], vec![], vec![], definitions)
    }

    #[test]
    fn compile_empty_module() {
        compile_module(&eir::ir::Module::new(
            vec![],
            vec![],
            vec![],
            vec![],
            vec![],
        ));
    }

    mod foreign_declarations {
        use super::*;

        #[test]
        fn compile() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![eir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::CallingConvention::Target,
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_with_multiple_arguments() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![eir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Function::new(
                            eir::types::Primitive::Float64,
                            eir::types::Primitive::Float64,
                        ),
                    ),
                    eir::ir::CallingConvention::Target,
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_with_source_calling_convention() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![eir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::CallingConvention::Source,
                )],
                vec![],
                vec![],
                vec![],
            ));
        }
    }

    mod foreign_definitions {
        use super::*;

        #[test]
        fn compile_for_foreign_declaration() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![eir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::CallingConvention::Target,
                )],
                vec![eir::ir::ForeignDefinition::new("f", "h")],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_for_declaration() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![],
                vec![eir::ir::ForeignDefinition::new("f", "g")],
                vec![eir::ir::Declaration::new(
                    "f",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Primitive::Float64,
                    ),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_for_definition() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![],
                vec![eir::ir::ForeignDefinition::new("f", "g")],
                vec![],
                vec![eir::ir::Definition::new(
                    "f",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::Variable::new("x"),
                    eir::types::Primitive::Float64,
                )],
            ));
        }
    }

    mod declarations {
        use super::*;

        #[test]
        fn compile() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![],
                vec![],
                vec![eir::ir::Declaration::new(
                    "f",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Primitive::Float64,
                    ),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_with_multiple_arguments() {
            compile_module(&eir::ir::Module::new(
                vec![],
                vec![],
                vec![],
                vec![eir::ir::Declaration::new(
                    "f",
                    eir::types::Function::new(
                        eir::types::Primitive::Float64,
                        eir::types::Function::new(
                            eir::types::Primitive::Float64,
                            eir::types::Primitive::Float64,
                        ),
                    ),
                )],
                vec![],
            ));
        }
    }

    mod definitions {
        use super::*;

        #[test]
        fn compile() {
            compile_module(&create_module_with_definitions(vec![
                eir::ir::Definition::new(
                    "f",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::Variable::new("x"),
                    eir::types::Primitive::Float64,
                ),
            ]));
        }

        #[test]
        fn compile_with_multiple_arguments() {
            compile_module(&create_module_with_definitions(vec![
                eir::ir::Definition::new(
                    "f",
                    vec![
                        eir::ir::Argument::new("x", eir::types::Primitive::Float64),
                        eir::ir::Argument::new("y", eir::types::Primitive::Float64),
                    ],
                    eir::ir::ArithmeticOperation::new(
                        eir::ir::ArithmeticOperator::Add,
                        eir::ir::Variable::new("x"),
                        eir::ir::Variable::new("y"),
                    ),
                    eir::types::Primitive::Float64,
                ),
            ]));
        }

        #[test]
        fn compile_thunk() {
            compile_module(&create_module_with_definitions(vec![
                eir::ir::Definition::thunk(
                    "f",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::Variable::new("x"),
                    eir::types::Primitive::Float64,
                ),
                eir::ir::Definition::new(
                    "g",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::FunctionApplication::new(
                        eir::ir::Variable::new("f"),
                        eir::ir::Variable::new("x"),
                    ),
                    eir::types::Primitive::Float64,
                ),
            ]));
        }
    }

    mod expressions {
        use super::*;

        #[test]
        fn compile_let() {
            compile_module(&create_module_with_definitions(vec![
                eir::ir::Definition::new(
                    "f",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::Let::new(
                        "y",
                        eir::types::Primitive::Float64,
                        eir::ir::Variable::new("x"),
                        eir::ir::Variable::new("y"),
                    ),
                    eir::types::Primitive::Float64,
                ),
            ]));
        }

        #[test]
        fn compile_let_recursive() {
            compile_module(&create_module_with_definitions(vec![
                eir::ir::Definition::new(
                    "f",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::LetRecursive::new(
                        vec![eir::ir::Definition::new(
                            "g",
                            vec![eir::ir::Argument::new("y", eir::types::Primitive::Float64)],
                            eir::ir::ArithmeticOperation::new(
                                eir::ir::ArithmeticOperator::Add,
                                eir::ir::Variable::new("x"),
                                eir::ir::Variable::new("y"),
                            ),
                            eir::types::Primitive::Float64,
                        )],
                        eir::ir::FunctionApplication::new(
                            eir::ir::Variable::new("g"),
                            eir::ir::Primitive::Float64(42.0),
                        ),
                    ),
                    eir::types::Primitive::Float64,
                ),
            ]));
        }

        #[test]
        fn compile_let_recursive_with_curried_function() {
            compile_module(&create_module_with_definitions(vec![
                eir::ir::Definition::new(
                    "f",
                    vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                    eir::ir::LetRecursive::new(
                        vec![eir::ir::Definition::new(
                            "g",
                            vec![eir::ir::Argument::new("y", eir::types::Primitive::Float64)],
                            eir::ir::LetRecursive::new(
                                vec![eir::ir::Definition::new(
                                    "h",
                                    vec![eir::ir::Argument::new(
                                        "z",
                                        eir::types::Primitive::Float64,
                                    )],
                                    eir::ir::ArithmeticOperation::new(
                                        eir::ir::ArithmeticOperator::Add,
                                        eir::ir::ArithmeticOperation::new(
                                            eir::ir::ArithmeticOperator::Add,
                                            eir::ir::Variable::new("x"),
                                            eir::ir::Variable::new("y"),
                                        ),
                                        eir::ir::Variable::new("z"),
                                    ),
                                    eir::types::Primitive::Float64,
                                )],
                                eir::ir::Variable::new("h"),
                            ),
                            eir::types::Function::new(
                                eir::types::Primitive::Float64,
                                eir::types::Primitive::Float64,
                            ),
                        )],
                        eir::ir::FunctionApplication::new(
                            eir::ir::FunctionApplication::new(
                                eir::ir::Variable::new("g"),
                                eir::ir::Primitive::Float64(42.0),
                            ),
                            eir::ir::Primitive::Float64(42.0),
                        ),
                    ),
                    eir::types::Primitive::Float64,
                ),
            ]));
        }

        mod variant_cases {
            use super::*;

            #[test]
            fn compile_with_float_64() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Type::Variant)],
                        eir::ir::VariantCase::new(
                            eir::ir::Variable::new("x"),
                            vec![eir::ir::VariantAlternative::new(
                                eir::types::Primitive::Float64,
                                "y",
                                eir::ir::Variable::new("y"),
                            )],
                            None,
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }

            #[test]
            fn compile_with_unboxed_record() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![eir::types::Primitive::Float64.into()]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Type::Variant)],
                        eir::ir::VariantCase::new(
                            eir::ir::Variable::new("x"),
                            vec![eir::ir::VariantAlternative::new(
                                reference_type.clone(),
                                "x",
                                eir::ir::Variable::new("x"),
                            )],
                            None,
                        ),
                        reference_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_boxed_record() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![eir::types::Primitive::Float64.into()]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Type::Variant)],
                        eir::ir::VariantCase::new(
                            eir::ir::Variable::new("x"),
                            vec![eir::ir::VariantAlternative::new(
                                reference_type.clone(),
                                "x",
                                eir::ir::Variable::new("x"),
                            )],
                            None,
                        ),
                        reference_type,
                    )],
                ));
            }
        }

        mod primitive_cases {
            use super::*;

            #[test]
            fn compile() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::PrimitiveCase::new(
                            eir::ir::Variable::new("x"),
                            vec![
                                eir::ir::PrimitiveAlternative::new(
                                    eir::ir::Primitive::Float64(0.0),
                                    eir::ir::Primitive::Float64(1.0),
                                ),
                                eir::ir::PrimitiveAlternative::new(
                                    eir::ir::Primitive::Float64(2.0),
                                    eir::ir::Primitive::Float64(3.0),
                                ),
                            ],
                            None,
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }

            #[test]
            fn compile_with_default_alternative() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::PrimitiveCase::new(
                            eir::ir::Variable::new("x"),
                            vec![
                                eir::ir::PrimitiveAlternative::new(
                                    eir::ir::Primitive::Float64(0.0),
                                    eir::ir::Primitive::Float64(1.0),
                                ),
                                eir::ir::PrimitiveAlternative::new(
                                    eir::ir::Primitive::Float64(2.0),
                                    eir::ir::Primitive::Float64(3.0),
                                ),
                            ],
                            Some(eir::ir::Primitive::Float64(4.0).into()),
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }
        }

        mod records {

            use super::*;

            #[test]
            fn compile_with_no_element() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::Record::new(reference_type.clone(), vec![]),
                        reference_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_1_element() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![eir::types::Primitive::Float64.into()]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::Record::new(
                            reference_type.clone(),
                            vec![eir::ir::Primitive::Float64(42.0).into()],
                        ),
                        reference_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_2_elements() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![
                            eir::types::Primitive::Float64.into(),
                            eir::types::Primitive::Integer64.into(),
                        ]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::Record::new(
                            reference_type.clone(),
                            vec![
                                eir::ir::Primitive::Float64(42.0).into(),
                                eir::ir::Primitive::Integer64(42).into(),
                            ],
                        ),
                        reference_type,
                    )],
                ));
            }

            #[test]
            fn compile_boxed() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![eir::types::Primitive::Float64.into()]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::Record::new(
                            reference_type.clone(),
                            vec![eir::ir::Primitive::Float64(42.0).into()],
                        ),
                        reference_type,
                    )],
                ));
            }
        }

        mod record_elements {
            use super::*;

            #[test]
            fn compile_with_1_element_record() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![eir::types::Primitive::Float64.into()]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", reference_type.clone())],
                        eir::ir::RecordElement::new(reference_type, 0, eir::ir::Variable::new("x")),
                        eir::types::Primitive::Float64,
                    )],
                ));
            }

            #[test]
            fn compile_with_2_element_record() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![
                            eir::types::Primitive::Boolean.into(),
                            eir::types::Primitive::Float64.into(),
                        ]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", reference_type.clone())],
                        eir::ir::RecordElement::new(reference_type, 1, eir::ir::Variable::new("x")),
                        eir::types::Primitive::Float64,
                    )],
                ));
            }
        }

        mod variants {
            use super::*;

            #[test]
            fn compile_with_float_64() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::Variant::new(
                            eir::types::Primitive::Float64,
                            eir::ir::Primitive::Float64(42.0),
                        ),
                        eir::types::Type::Variant,
                    ),
                ]));
            }

            #[test]
            fn compile_with_empty_unboxed_record() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", reference_type.clone())],
                        eir::ir::Variant::new(
                            reference_type.clone(),
                            eir::ir::Record::new(reference_type, vec![]),
                        ),
                        eir::types::Type::Variant,
                    )],
                ));
            }

            #[test]
            fn compile_with_unboxed_record() {
                let reference_type = eir::types::Reference::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![eir::ir::TypeDefinition::new(
                        "foo",
                        eir::types::Record::new(vec![eir::types::Primitive::Float64.into()]),
                    )],
                    vec![eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", reference_type.clone())],
                        eir::ir::Variant::new(
                            reference_type.clone(),
                            eir::ir::Record::new(
                                reference_type,
                                vec![eir::ir::Primitive::Float64(42.0).into()],
                            ),
                        ),
                        eir::types::Type::Variant,
                    )],
                ));
            }
        }

        mod function_applications {
            use super::*;

            #[test]
            fn compile_1_argument() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::Variable::new("x"),
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::Variable::new("f"),
                            eir::ir::Primitive::Float64(42.0),
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }

            #[test]
            fn compile_2_arguments() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![
                            eir::ir::Argument::new("x", eir::types::Primitive::Float64),
                            eir::ir::Argument::new("y", eir::types::Primitive::Integer32),
                        ],
                        eir::ir::Variable::new("x"),
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::FunctionApplication::new(
                                eir::ir::Variable::new("f"),
                                eir::ir::Primitive::Float64(42.0),
                            ),
                            eir::ir::Primitive::Integer32(42),
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }

            #[test]
            fn compile_3_arguments() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![
                            eir::ir::Argument::new("x", eir::types::Primitive::Float64),
                            eir::ir::Argument::new("y", eir::types::Primitive::Integer32),
                            eir::ir::Argument::new("z", eir::types::Primitive::Integer64),
                        ],
                        eir::ir::Variable::new("x"),
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::FunctionApplication::new(
                                eir::ir::FunctionApplication::new(
                                    eir::ir::Variable::new("f"),
                                    eir::ir::Primitive::Float64(111.0),
                                ),
                                eir::ir::Primitive::Integer32(222),
                            ),
                            eir::ir::Primitive::Integer64(333),
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }

            #[test]
            fn compile_1_argument_with_arity_of_2() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![
                            eir::ir::Argument::new("x", eir::types::Primitive::Float64),
                            eir::ir::Argument::new("y", eir::types::Primitive::Integer32),
                        ],
                        eir::ir::Variable::new("x"),
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::Variable::new("f"),
                            eir::ir::Primitive::Float64(42.0),
                        ),
                        eir::types::Function::new(
                            eir::types::Primitive::Integer32,
                            eir::types::Primitive::Float64,
                        ),
                    ),
                ]));
            }

            #[test]
            fn compile_1_argument_with_arity_of_3() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![
                            eir::ir::Argument::new("x", eir::types::Primitive::Float64),
                            eir::ir::Argument::new("y", eir::types::Primitive::Integer32),
                            eir::ir::Argument::new("z", eir::types::Primitive::Integer64),
                        ],
                        eir::ir::Variable::new("x"),
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::Variable::new("f"),
                            eir::ir::Primitive::Float64(42.0),
                        ),
                        eir::types::Function::new(
                            eir::types::Primitive::Integer32,
                            eir::types::Function::new(
                                eir::types::Primitive::Integer64,
                                eir::types::Primitive::Float64,
                            ),
                        ),
                    ),
                ]));
            }

            #[test]
            fn compile_2_arguments_with_arity_of_3() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![
                            eir::ir::Argument::new("x", eir::types::Primitive::Float64),
                            eir::ir::Argument::new("y", eir::types::Primitive::Integer32),
                            eir::ir::Argument::new("z", eir::types::Primitive::Integer64),
                        ],
                        eir::ir::Variable::new("x"),
                        eir::types::Primitive::Float64,
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::FunctionApplication::new(
                                eir::ir::Variable::new("f"),
                                eir::ir::Primitive::Float64(111.0),
                            ),
                            eir::ir::Primitive::Integer32(222),
                        ),
                        eir::types::Function::new(
                            eir::types::Primitive::Integer64,
                            eir::types::Primitive::Float64,
                        ),
                    ),
                ]));
            }

            #[test]
            fn compile_with_curried_function() {
                compile_module(&create_module_with_definitions(vec![
                    eir::ir::Definition::new(
                        "f",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::LetRecursive::new(
                            vec![eir::ir::Definition::new(
                                "g",
                                vec![eir::ir::Argument::new("y", eir::types::Primitive::Float64)],
                                eir::ir::ArithmeticOperation::new(
                                    eir::ir::ArithmeticOperator::Add,
                                    eir::ir::Variable::new("x"),
                                    eir::ir::Variable::new("y"),
                                ),
                                eir::types::Primitive::Float64,
                            )],
                            eir::ir::Variable::new("g"),
                        ),
                        eir::types::Function::new(
                            eir::types::Primitive::Float64,
                            eir::types::Primitive::Float64,
                        ),
                    ),
                    eir::ir::Definition::new(
                        "g",
                        vec![eir::ir::Argument::new("x", eir::types::Primitive::Float64)],
                        eir::ir::FunctionApplication::new(
                            eir::ir::FunctionApplication::new(
                                eir::ir::Variable::new("f"),
                                eir::ir::Primitive::Float64(111.0),
                            ),
                            eir::ir::Primitive::Float64(222.0),
                        ),
                        eir::types::Primitive::Float64,
                    ),
                ]));
            }
        }
    }
}
