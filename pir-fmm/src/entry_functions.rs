use crate::{expressions, types};
use std::collections::HashMap;

const ENVIRONMENT_NAME: &str = "_env";

pub fn compile(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    Ok(if definition.is_thunk() {
        compile_thunk(module_builder, definition, variables, types)?
    } else {
        compile_non_thunk(module_builder, definition, variables, types)?
    })
}

fn compile_non_thunk(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    module_builder.define_anonymous_function(
        compile_arguments(definition, types),
        |instruction_builder| {
            Ok(instruction_builder.return_(compile_body(
                module_builder,
                &instruction_builder,
                definition,
                variables,
                types,
            )?))
        },
        types::compile(definition.result_type(), types),
        fmm::types::CallingConvention::Source,
    )
}

fn compile_thunk(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    compile_first_thunk_entry(
        module_builder,
        definition,
        compile_normal_thunk_entry(module_builder, definition, types)?,
        compile_locked_thunk_entry(module_builder, definition, types)?,
        variables,
        types,
    )
}

fn compile_body(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    definition: &pir::ir::Definition,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    expressions::compile(
        module_builder,
        instruction_builder,
        definition.body(),
        &variables
            .clone()
            .into_iter()
            .chain(
                definition
                    .environment()
                    .iter()
                    .enumerate()
                    .map(|(index, free_variable)| {
                        Ok((
                            free_variable.name().into(),
                            instruction_builder.load(instruction_builder.record_address(
                                fmm::build::bit_cast(
                                    fmm::types::Pointer::new(types::compile_environment(
                                        definition, types,
                                    )),
                                    compile_environment_pointer(),
                                ),
                                index,
                            )?)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .chain(definition.arguments().iter().map(|argument| {
                (
                    argument.name().into(),
                    fmm::build::variable(argument.name(), types::compile(argument.type_(), types)),
                )
            }))
            .collect(),
        types,
    )
}

fn compile_first_thunk_entry(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    normal_entry_function: fmm::build::TypedExpression,
    lock_entry_function: fmm::build::TypedExpression,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let entry_function_name = module_builder.generate_name();
    let entry_function_type = types::compile_entry_function_from_definition(definition, types);
    let arguments = compile_arguments(definition, types);

    module_builder.define_function(
        &entry_function_name,
        arguments.clone(),
        |instruction_builder| {
            instruction_builder.if_(
                instruction_builder.compare_and_swap(
                    compile_entry_function_pointer_pointer(
                        &instruction_builder,
                        definition,
                        types,
                    )?,
                    fmm::build::variable(&entry_function_name, entry_function_type.clone()),
                    lock_entry_function.clone(),
                ),
                |instruction_builder| {
                    let value = compile_body(
                        module_builder,
                        &instruction_builder,
                        definition,
                        variables,
                        types,
                    )?;

                    instruction_builder.store(
                        value.clone(),
                        fmm::build::bit_cast(
                            fmm::types::Pointer::new(types::compile(
                                definition.result_type(),
                                types,
                            )),
                            compile_environment_pointer(),
                        ),
                    );
                    instruction_builder.atomic_store(
                        normal_entry_function.clone(),
                        compile_entry_function_pointer_pointer(
                            &instruction_builder,
                            definition,
                            types,
                        )?,
                    );

                    Ok(instruction_builder.return_(value))
                },
                |instruction_builder| {
                    Ok(instruction_builder.return_(
                        instruction_builder.call(
                            instruction_builder.atomic_load(
                                compile_entry_function_pointer_pointer(
                                    &instruction_builder,
                                    definition,
                                    types,
                                )?,
                            )?,
                            arguments
                                .iter()
                                .map(|argument| {
                                    fmm::build::variable(argument.name(), argument.type_().clone())
                                })
                                .collect(),
                        )?,
                    ))
                },
            )?;

            Ok(instruction_builder.unreachable())
        },
        types::compile(definition.result_type(), types),
        fmm::types::CallingConvention::Source,
        fmm::ir::Linkage::Internal,
    )
}

fn compile_normal_thunk_entry(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    module_builder.define_anonymous_function(
        compile_arguments(definition, types),
        |instruction_builder| compile_normal_body(&instruction_builder, definition, types),
        types::compile(definition.result_type(), types),
        fmm::types::CallingConvention::Source,
    )
}

fn compile_locked_thunk_entry(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let entry_function_name = module_builder.generate_name();

    module_builder.define_function(
        &entry_function_name,
        compile_arguments(definition, types),
        |instruction_builder| {
            instruction_builder.if_(
                fmm::build::comparison_operation(
                    fmm::ir::ComparisonOperator::Equal,
                    fmm::build::bit_cast(
                        fmm::types::Primitive::PointerInteger,
                        instruction_builder.atomic_load(compile_entry_function_pointer_pointer(
                            &instruction_builder,
                            definition,
                            types,
                        )?)?,
                    ),
                    fmm::build::bit_cast(
                        fmm::types::Primitive::PointerInteger,
                        fmm::build::variable(
                            &entry_function_name,
                            types::compile_entry_function_from_definition(definition, types),
                        ),
                    ),
                )?,
                // TODO Return to handle thunk locks asynchronously.
                |instruction_builder| Ok(instruction_builder.unreachable()),
                |instruction_builder| compile_normal_body(&instruction_builder, definition, types),
            )?;

            Ok(instruction_builder.unreachable())
        },
        types::compile(definition.result_type(), types),
        fmm::types::CallingConvention::Source,
        fmm::ir::Linkage::Internal,
    )
}

fn compile_normal_body(
    instruction_builder: &fmm::build::InstructionBuilder,
    definition: &pir::ir::Definition,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::ir::Block, fmm::build::BuildError> {
    Ok(
        instruction_builder.return_(instruction_builder.load(fmm::build::bit_cast(
            fmm::types::Pointer::new(types::compile(definition.result_type(), types)),
            compile_environment_pointer(),
        ))?),
    )
}

fn compile_entry_function_pointer_pointer(
    instruction_builder: &fmm::build::InstructionBuilder,
    definition: &pir::ir::Definition,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    // TODO Calculate entry function pointer properly.
    // The offset should be calculated by allocating a record of
    // { pointer, { pointer, arity, environment } }.
    instruction_builder.pointer_address(
        fmm::build::bit_cast(
            fmm::types::Pointer::new(types::compile_entry_function_from_definition(
                definition, types,
            )),
            compile_environment_pointer(),
        ),
        fmm::ir::Primitive::PointerInteger(-2),
    )
}

fn compile_arguments(
    definition: &pir::ir::Definition,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Vec<fmm::ir::Argument> {
    vec![fmm::ir::Argument::new(
        ENVIRONMENT_NAME,
        fmm::types::Pointer::new(types::compile_unsized_environment()),
    )]
    .into_iter()
    .chain(definition.arguments().iter().map(|argument| {
        fmm::ir::Argument::new(argument.name(), types::compile(argument.type_(), types))
    }))
    .collect()
}

fn compile_environment_pointer() -> fmm::build::TypedExpression {
    fmm::build::variable(
        ENVIRONMENT_NAME,
        fmm::types::Pointer::new(types::compile_unsized_environment()),
    )
}
