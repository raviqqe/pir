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

pub fn compile(module: &pir::ir::Module) -> Result<fmm::ir::Module, CompileError> {
    pir::analysis::check_types(module)?;

    let module = pir::analysis::infer_environment(module);

    let module_builder = fmm::build::ModuleBuilder::new();
    let types = module
        .type_definitions()
        .iter()
        .map(|definition| (definition.name().into(), definition.type_().clone()))
        .collect();

    for type_ in &pir::analysis::collect_variant_types(&module) {
        compile_type_information_global_variable(&module_builder, type_)?;
    }

    for declaration in module.foreign_declarations() {
        compile_foreign_declaration(&module_builder, declaration, &types)?;
    }

    for declaration in module.declarations() {
        compile_declaration(&module_builder, declaration, &types);
    }

    let global_variables = compile_global_variables(&module, &types);

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
    module: &pir::ir::Module,
    types: &HashMap<String, pir::types::RecordBody>,
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

    fn compile_module(module: &pir::ir::Module) {
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

    fn create_module_with_definitions(definitions: Vec<pir::ir::Definition>) -> pir::ir::Module {
        pir::ir::Module::new(vec![], vec![], vec![], vec![], definitions)
    }

    fn create_module_with_type_definitions(
        variant_definitions: Vec<pir::ir::TypeDefinition>,
        definitions: Vec<pir::ir::Definition>,
    ) -> pir::ir::Module {
        pir::ir::Module::new(variant_definitions, vec![], vec![], vec![], definitions)
    }

    #[test]
    fn compile_empty_module() {
        compile_module(&pir::ir::Module::new(
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
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![pir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::CallingConvention::Target,
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_with_multiple_arguments() {
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![pir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Function::new(
                            pir::types::Primitive::Number,
                            pir::types::Primitive::Number,
                        ),
                    ),
                    pir::ir::CallingConvention::Target,
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_with_source_calling_convention() {
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![pir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::CallingConvention::Source,
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
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![pir::ir::ForeignDeclaration::new(
                    "f",
                    "g",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::CallingConvention::Target,
                )],
                vec![pir::ir::ForeignDefinition::new("f", "h")],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_for_declaration() {
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![],
                vec![pir::ir::ForeignDefinition::new("f", "g")],
                vec![pir::ir::Declaration::new(
                    "f",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Primitive::Number,
                    ),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_for_definition() {
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![],
                vec![pir::ir::ForeignDefinition::new("f", "g")],
                vec![],
                vec![pir::ir::Definition::new(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::Variable::new("x"),
                    pir::types::Primitive::Number,
                )],
            ));
        }
    }

    mod declarations {
        use super::*;

        #[test]
        fn compile() {
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![],
                vec![],
                vec![pir::ir::Declaration::new(
                    "f",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Primitive::Number,
                    ),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_with_multiple_arguments() {
            compile_module(&pir::ir::Module::new(
                vec![],
                vec![],
                vec![],
                vec![pir::ir::Declaration::new(
                    "f",
                    pir::types::Function::new(
                        pir::types::Primitive::Number,
                        pir::types::Function::new(
                            pir::types::Primitive::Number,
                            pir::types::Primitive::Number,
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
                pir::ir::Definition::new(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::Variable::new("x"),
                    pir::types::Primitive::Number,
                ),
            ]));
        }

        #[test]
        fn compile_with_multiple_arguments() {
            compile_module(&create_module_with_definitions(vec![
                pir::ir::Definition::new(
                    "f",
                    vec![
                        pir::ir::Argument::new("x", pir::types::Primitive::Number),
                        pir::ir::Argument::new("y", pir::types::Primitive::Number),
                    ],
                    pir::ir::ArithmeticOperation::new(
                        pir::ir::ArithmeticOperator::Add,
                        pir::ir::Variable::new("x"),
                        pir::ir::Variable::new("y"),
                    ),
                    pir::types::Primitive::Number,
                ),
            ]));
        }

        #[test]
        fn compile_thunk() {
            compile_module(&create_module_with_definitions(vec![
                pir::ir::Definition::thunk(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::Variable::new("x"),
                    pir::types::Primitive::Number,
                ),
                pir::ir::Definition::new(
                    "g",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::FunctionApplication::new(
                        pir::ir::Variable::new("f"),
                        pir::ir::Variable::new("x"),
                    ),
                    pir::types::Primitive::Number,
                ),
            ]));
        }
    }

    mod expressions {
        use super::*;

        #[test]
        fn compile_let() {
            compile_module(&create_module_with_definitions(vec![
                pir::ir::Definition::new(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::Let::new(
                        "y",
                        pir::types::Primitive::Number,
                        pir::ir::Variable::new("x"),
                        pir::ir::Variable::new("y"),
                    ),
                    pir::types::Primitive::Number,
                ),
            ]));
        }

        #[test]
        fn compile_let_recursive() {
            compile_module(&create_module_with_definitions(vec![
                pir::ir::Definition::new(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::LetRecursive::new(
                        vec![pir::ir::Definition::new(
                            "g",
                            vec![pir::ir::Argument::new("y", pir::types::Primitive::Number)],
                            pir::ir::ArithmeticOperation::new(
                                pir::ir::ArithmeticOperator::Add,
                                pir::ir::Variable::new("x"),
                                pir::ir::Variable::new("y"),
                            ),
                            pir::types::Primitive::Number,
                        )],
                        pir::ir::FunctionApplication::new(
                            pir::ir::Variable::new("g"),
                            pir::ir::Primitive::Number(42.0),
                        ),
                    ),
                    pir::types::Primitive::Number,
                ),
            ]));
        }

        #[test]
        fn compile_let_recursive_with_curried_function() {
            compile_module(&create_module_with_definitions(vec![
                pir::ir::Definition::new(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::LetRecursive::new(
                        vec![pir::ir::Definition::new(
                            "g",
                            vec![pir::ir::Argument::new("y", pir::types::Primitive::Number)],
                            pir::ir::LetRecursive::new(
                                vec![pir::ir::Definition::new(
                                    "h",
                                    vec![pir::ir::Argument::new(
                                        "z",
                                        pir::types::Primitive::Number,
                                    )],
                                    pir::ir::ArithmeticOperation::new(
                                        pir::ir::ArithmeticOperator::Add,
                                        pir::ir::ArithmeticOperation::new(
                                            pir::ir::ArithmeticOperator::Add,
                                            pir::ir::Variable::new("x"),
                                            pir::ir::Variable::new("y"),
                                        ),
                                        pir::ir::Variable::new("z"),
                                    ),
                                    pir::types::Primitive::Number,
                                )],
                                pir::ir::Variable::new("h"),
                            ),
                            pir::types::Function::new(
                                pir::types::Primitive::Number,
                                pir::types::Primitive::Number,
                            ),
                        )],
                        pir::ir::FunctionApplication::new(
                            pir::ir::FunctionApplication::new(
                                pir::ir::Variable::new("g"),
                                pir::ir::Primitive::Number(42.0),
                            ),
                            pir::ir::Primitive::Number(42.0),
                        ),
                    ),
                    pir::types::Primitive::Number,
                ),
            ]));
        }

        mod cases {
            use super::*;

            #[test]
            fn compile_with_float_64() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Type::Variant)],
                        pir::ir::Case::new(
                            pir::ir::Variable::new("x"),
                            vec![pir::ir::VariantAlternative::new(
                                pir::types::Primitive::Number,
                                "y",
                                pir::ir::Variable::new("y"),
                            )],
                            None,
                        ),
                        pir::types::Primitive::Number,
                    ),
                ]));
            }

            #[test]
            fn compile_with_unboxed_record() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![pir::types::Primitive::Number.into()]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Type::Variant)],
                        pir::ir::Case::new(
                            pir::ir::Variable::new("x"),
                            vec![pir::ir::VariantAlternative::new(
                                record_type.clone(),
                                "x",
                                pir::ir::Variable::new("x"),
                            )],
                            None,
                        ),
                        record_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_boxed_record() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![pir::types::Primitive::Number.into()]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Type::Variant)],
                        pir::ir::Case::new(
                            pir::ir::Variable::new("x"),
                            vec![pir::ir::VariantAlternative::new(
                                record_type.clone(),
                                "x",
                                pir::ir::Variable::new("x"),
                            )],
                            None,
                        ),
                        record_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_string() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Type::Variant)],
                        pir::ir::Case::new(
                            pir::ir::Variable::new("x"),
                            vec![pir::ir::VariantAlternative::new(
                                pir::types::Type::String,
                                "y",
                                pir::ir::Variable::new("y"),
                            )],
                            None,
                        ),
                        pir::types::Type::String,
                    ),
                ]));
            }
        }

        mod records {
            use super::*;

            #[test]
            fn compile_with_no_element() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Record::new(record_type.clone(), vec![]),
                        record_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_1_element() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![pir::types::Primitive::Number.into()]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Record::new(
                            record_type.clone(),
                            vec![pir::ir::Primitive::Number(42.0).into()],
                        ),
                        record_type,
                    )],
                ));
            }

            #[test]
            fn compile_with_2_elements() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![
                            pir::types::Primitive::Number.into(),
                            pir::types::Primitive::Boolean.into(),
                        ]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Record::new(record_type.clone(), vec![42.0.into(), true.into()]),
                        record_type,
                    )],
                ));
            }

            #[test]
            fn compile_boxed() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![pir::types::Primitive::Number.into()]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Record::new(
                            record_type.clone(),
                            vec![pir::ir::Primitive::Number(42.0).into()],
                        ),
                        record_type,
                    )],
                ));
            }
        }

        mod record_elements {
            use super::*;

            #[test]
            fn compile_with_1_element_record() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![pir::types::Primitive::Number.into()]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", record_type.clone())],
                        pir::ir::RecordElement::new(record_type, 0, pir::ir::Variable::new("x")),
                        pir::types::Primitive::Number,
                    )],
                ));
            }

            #[test]
            fn compile_with_2_element_record() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![
                            pir::types::Primitive::Boolean.into(),
                            pir::types::Primitive::Number.into(),
                        ]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", record_type.clone())],
                        pir::ir::RecordElement::new(record_type, 1, pir::ir::Variable::new("x")),
                        pir::types::Primitive::Number,
                    )],
                ));
            }
        }

        mod variants {
            use super::*;

            #[test]
            fn compile_with_float_64() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Variant::new(
                            pir::types::Primitive::Number,
                            pir::ir::Primitive::Number(42.0),
                        ),
                        pir::types::Type::Variant,
                    ),
                ]));
            }

            #[test]
            fn compile_with_empty_unboxed_record() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", record_type.clone())],
                        pir::ir::Variant::new(
                            record_type.clone(),
                            pir::ir::Record::new(record_type, vec![]),
                        ),
                        pir::types::Type::Variant,
                    )],
                ));
            }

            #[test]
            fn compile_with_unboxed_record() {
                let record_type = pir::types::Record::new("foo");

                compile_module(&create_module_with_type_definitions(
                    vec![pir::ir::TypeDefinition::new(
                        "foo",
                        pir::types::RecordBody::new(vec![pir::types::Primitive::Number.into()]),
                    )],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", record_type.clone())],
                        pir::ir::Variant::new(
                            record_type.clone(),
                            pir::ir::Record::new(
                                record_type,
                                vec![pir::ir::Primitive::Number(42.0).into()],
                            ),
                        ),
                        pir::types::Type::Variant,
                    )],
                ));
            }

            #[test]
            fn compile_with_string() {
                compile_module(&create_module_with_type_definitions(
                    vec![],
                    vec![pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Variant::new(
                            pir::types::Type::String,
                            pir::ir::PirString::new("foo"),
                        ),
                        pir::types::Type::Variant,
                    )],
                ));
            }
        }

        mod function_applications {
            use super::*;

            #[test]
            fn compile_1_argument() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::Variable::new("x"),
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::Variable::new("f"),
                            pir::ir::Primitive::Number(42.0),
                        ),
                        pir::types::Primitive::Number,
                    ),
                ]));
            }

            #[test]
            fn compile_2_arguments() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![
                            pir::ir::Argument::new("x", pir::types::Primitive::Number),
                            pir::ir::Argument::new("y", pir::types::Primitive::Boolean),
                        ],
                        pir::ir::Variable::new("x"),
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::FunctionApplication::new(pir::ir::Variable::new("f"), 42.0),
                            true,
                        ),
                        pir::types::Primitive::Number,
                    ),
                ]));
            }

            #[test]
            fn compile_3_arguments() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![
                            pir::ir::Argument::new("x", pir::types::Primitive::Number),
                            pir::ir::Argument::new("y", pir::types::Primitive::Boolean),
                            pir::ir::Argument::new("z", pir::types::Type::String),
                        ],
                        pir::ir::Variable::new("x"),
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::FunctionApplication::new(
                                pir::ir::FunctionApplication::new(
                                    pir::ir::Variable::new("f"),
                                    42.0,
                                ),
                                true,
                            ),
                            pir::ir::PirString::new("foo"),
                        ),
                        pir::types::Primitive::Number,
                    ),
                ]));
            }

            #[test]
            fn compile_1_argument_with_arity_of_2() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![
                            pir::ir::Argument::new("x", pir::types::Primitive::Number),
                            pir::ir::Argument::new("y", pir::types::Primitive::Boolean),
                        ],
                        pir::ir::Variable::new("x"),
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::Variable::new("f"),
                            pir::ir::Primitive::Number(42.0),
                        ),
                        pir::types::Function::new(
                            pir::types::Primitive::Boolean,
                            pir::types::Primitive::Number,
                        ),
                    ),
                ]));
            }

            #[test]
            fn compile_1_argument_with_arity_of_3() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![
                            pir::ir::Argument::new("x", pir::types::Primitive::Number),
                            pir::ir::Argument::new("y", pir::types::Primitive::Boolean),
                            pir::ir::Argument::new("z", pir::types::Type::String),
                        ],
                        pir::ir::Variable::new("x"),
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::Variable::new("f"),
                            pir::ir::Primitive::Number(42.0),
                        ),
                        pir::types::Function::new(
                            pir::types::Primitive::Boolean,
                            pir::types::Function::new(
                                pir::types::Type::String,
                                pir::types::Primitive::Number,
                            ),
                        ),
                    ),
                ]));
            }

            #[test]
            fn compile_2_arguments_with_arity_of_3() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![
                            pir::ir::Argument::new("x", pir::types::Primitive::Number),
                            pir::ir::Argument::new("y", pir::types::Primitive::Boolean),
                            pir::ir::Argument::new("z", pir::types::Type::String),
                        ],
                        pir::ir::Variable::new("x"),
                        pir::types::Primitive::Number,
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::FunctionApplication::new(pir::ir::Variable::new("f"), 42.0),
                            true,
                        ),
                        pir::types::Function::new(
                            pir::types::Type::String,
                            pir::types::Primitive::Number,
                        ),
                    ),
                ]));
            }

            #[test]
            fn compile_with_curried_function() {
                compile_module(&create_module_with_definitions(vec![
                    pir::ir::Definition::new(
                        "f",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::LetRecursive::new(
                            vec![pir::ir::Definition::new(
                                "g",
                                vec![pir::ir::Argument::new("y", pir::types::Primitive::Number)],
                                pir::ir::ArithmeticOperation::new(
                                    pir::ir::ArithmeticOperator::Add,
                                    pir::ir::Variable::new("x"),
                                    pir::ir::Variable::new("y"),
                                ),
                                pir::types::Primitive::Number,
                            )],
                            pir::ir::Variable::new("g"),
                        ),
                        pir::types::Function::new(
                            pir::types::Primitive::Number,
                            pir::types::Primitive::Number,
                        ),
                    ),
                    pir::ir::Definition::new(
                        "g",
                        vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                        pir::ir::FunctionApplication::new(
                            pir::ir::FunctionApplication::new(
                                pir::ir::Variable::new("f"),
                                pir::ir::Primitive::Number(111.0),
                            ),
                            pir::ir::Primitive::Number(222.0),
                        ),
                        pir::types::Primitive::Number,
                    ),
                ]));
            }
        }

        #[test]
        fn compile_if() {
            compile_module(&create_module_with_definitions(vec![
                pir::ir::Definition::new(
                    "f",
                    vec![pir::ir::Argument::new("x", pir::types::Primitive::Number)],
                    pir::ir::If::new(true, 42.0, 42.0),
                    pir::types::Primitive::Number,
                ),
            ]));
        }
    }
}
