use crate::closures;
use crate::entry_functions;
use crate::function_applications;
use crate::types;
use std::collections::HashMap;

pub fn compile_arity(arity: usize) -> fmm::ir::Primitive {
    fmm::ir::Primitive::PointerInteger(arity as i64)
}

pub fn compile(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    expression: &ssf::ir::Expression,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let compile = |expression, variables| {
        compile(
            module_builder,
            instruction_builder,
            expression,
            variables,
            types,
        )
    };

    Ok(match expression {
        ssf::ir::Expression::ArithmeticOperation(operation) => compile_arithmetic_operation(
            module_builder,
            instruction_builder,
            operation,
            variables,
            types,
        )?
        .into(),
        ssf::ir::Expression::Case(case) => {
            compile_case(module_builder, instruction_builder, case, variables, types)?
        }
        ssf::ir::Expression::ComparisonOperation(operation) => compile_comparison_operation(
            module_builder,
            instruction_builder,
            operation,
            variables,
            types,
        )?
        .into(),
        ssf::ir::Expression::FunctionApplication(function_application) => {
            function_applications::compile(
                module_builder,
                instruction_builder,
                compile(function_application.first_function(), variables)?,
                &function_application
                    .arguments()
                    .into_iter()
                    .map(|argument| compile(argument, variables))
                    .collect::<Result<Vec<_>, _>>()?,
            )?
        }
        ssf::ir::Expression::Let(let_) => {
            compile_let(module_builder, instruction_builder, let_, variables, types)?
        }
        ssf::ir::Expression::LetRecursive(let_recursive) => compile_let_recursive(
            module_builder,
            instruction_builder,
            let_recursive,
            variables,
            types,
        )?,
        ssf::ir::Expression::Primitive(primitive) => compile_primitive(primitive).into(),
        ssf::ir::Expression::Record(record) => {
            let unboxed = fmm::build::record(
                record
                    .elements()
                    .iter()
                    .map(|argument| compile(argument, variables))
                    .collect::<Result<_, _>>()?,
            );

            if types::is_reference_boxed(record.type_(), types) {
                let pointer = instruction_builder.allocate_heap(unboxed.type_().clone());

                instruction_builder.store(unboxed, pointer.clone());

                fmm::build::bit_cast(types::compile_reference(record.type_(), types), pointer)
                    .into()
            } else {
                unboxed.into()
            }
        }
        ssf::ir::Expression::RecordElement(element) => {
            let record = compile(element.record(), variables)?;

            instruction_builder.deconstruct_record(
                if types::is_reference_boxed(element.type_(), types) {
                    instruction_builder.load(fmm::build::bit_cast(
                        fmm::types::Pointer::new(types::compile_unboxed_reference(
                            element.type_(),
                            types,
                        )),
                        record,
                    ))?
                } else {
                    record
                },
                element.index(),
            )?
        }
        ssf::ir::Expression::Variable(variable) => variables[variable.name()].clone(),
        ssf::ir::Expression::Variant(variant) => fmm::build::record(vec![
            compile_variant_tag(variant.type_()),
            compile_payload_bit_cast(
                instruction_builder,
                types::compile_payload(),
                compile(variant.payload(), variables)?,
            )?,
        ])
        .into(),
    })
}

fn compile_case(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    case: &ssf::ir::Case,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    Ok(match case {
        ssf::ir::Case::Primitive(case) => {
            compile_primitive_case(module_builder, instruction_builder, case, variables, types)?
        }
        ssf::ir::Case::Variant(case) => {
            compile_variant_case(module_builder, instruction_builder, case, variables, types)?
        }
    })
}

fn compile_variant_case(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    case: &ssf::ir::VariantCase,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let argument = compile(
        module_builder,
        instruction_builder,
        case.argument(),
        variables,
        types,
    )?;

    Ok(compile_variant_alternatives(
        module_builder,
        instruction_builder,
        argument,
        case.alternatives(),
        case.default_alternative(),
        variables,
        types,
    )?
    .unwrap())
}

fn compile_variant_alternatives(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    argument: fmm::build::TypedExpression,
    alternatives: &[ssf::ir::VariantAlternative],
    default_alternative: Option<&ssf::ir::Expression>,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<Option<fmm::build::TypedExpression>, fmm::build::BuildError> {
    Ok(match alternatives {
        [] => default_alternative
            .map(|default_alternative| {
                compile(
                    module_builder,
                    instruction_builder,
                    default_alternative,
                    variables,
                    types,
                )
            })
            .transpose()?,
        [alternative, ..] => Some(instruction_builder.if_(
            fmm::build::comparison_operation(
                fmm::ir::ComparisonOperator::Equal,
                fmm::build::bit_cast(
                    fmm::types::Primitive::PointerInteger,
                    instruction_builder.deconstruct_record(argument.clone(), 0)?,
                ),
                fmm::build::bit_cast(
                    fmm::types::Primitive::PointerInteger,
                    compile_variant_tag(alternative.type_()),
                ),
            )?,
            |instruction_builder| {
                Ok(instruction_builder.branch(compile(
                    module_builder,
                    &instruction_builder,
                    alternative.expression(),
                    &variables
                        .clone()
                        .into_iter()
                        .chain(vec![(
                            alternative.name().into(),
                            compile_payload_bit_cast(
                                &instruction_builder,
                                types::compile(alternative.type_(), types),
                                instruction_builder.deconstruct_record(argument.clone(), 1)?,
                            )?,
                        )])
                        .collect(),
                    types,
                )?))
            },
            |instruction_builder| {
                Ok(
                    if let Some(expression) = compile_variant_alternatives(
                        module_builder,
                        &instruction_builder,
                        argument.clone(),
                        &alternatives[1..],
                        default_alternative,
                        variables,
                        types,
                    )? {
                        instruction_builder.branch(expression)
                    } else {
                        instruction_builder.unreachable()
                    },
                )
            },
        )?),
    })
}

fn compile_variant_tag(type_: &ssf::types::Type) -> fmm::build::TypedExpression {
    fmm::build::variable(types::compile_type_id(type_), types::compile_variant_tag())
}

fn compile_payload_bit_cast(
    builder: &fmm::build::InstructionBuilder,
    to_type: impl Into<fmm::types::Type>,
    argument: impl Into<fmm::build::TypedExpression>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let argument = argument.into();
    let to_type = to_type.into();

    Ok(if argument.type_() == &to_type {
        argument
    } else {
        builder.deconstruct_union(
            fmm::ir::Union::new(
                fmm::types::Union::new(vec![argument.type_().clone(), to_type]),
                0,
                argument.expression().clone(),
            ),
            1,
        )?
    })
}

fn compile_primitive_case(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    case: &ssf::ir::PrimitiveCase,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let argument = compile(
        module_builder,
        instruction_builder,
        case.argument(),
        variables,
        types,
    )?;

    Ok(compile_primitive_alternatives(
        module_builder,
        instruction_builder,
        argument,
        case.alternatives(),
        case.default_alternative(),
        variables,
        types,
    )?
    .unwrap())
}

fn compile_primitive_alternatives(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    argument: fmm::build::TypedExpression,
    alternatives: &[ssf::ir::PrimitiveAlternative],
    default_alternative: Option<&ssf::ir::Expression>,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<Option<fmm::build::TypedExpression>, fmm::build::BuildError> {
    let compile = |expression| {
        compile(
            module_builder,
            instruction_builder,
            expression,
            variables,
            types,
        )
    };

    Ok(match alternatives {
        [] => default_alternative.map(compile).transpose()?,
        [alternative, ..] => Some(instruction_builder.if_(
            fmm::build::comparison_operation(
                fmm::ir::ComparisonOperator::Equal,
                argument.clone(),
                compile_primitive(alternative.primitive()),
            )?,
            |instruction_builder| {
                Ok(instruction_builder.branch(compile(alternative.expression())?))
            },
            |instruction_builder| {
                Ok(
                    if let Some(expression) = compile_primitive_alternatives(
                        module_builder,
                        &instruction_builder,
                        argument.clone(),
                        &alternatives[1..],
                        default_alternative,
                        variables,
                        types,
                    )? {
                        instruction_builder.branch(expression)
                    } else {
                        instruction_builder.unreachable()
                    },
                )
            },
        )?),
    })
}

fn compile_let(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    let_: &ssf::ir::Let,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let compile = |expression, variables| {
        compile(
            module_builder,
            instruction_builder,
            expression,
            variables,
            types,
        )
    };

    compile(
        let_.expression(),
        &variables
            .clone()
            .drain()
            .chain(vec![(
                let_.name().into(),
                compile(let_.bound_expression(), variables)?,
            )])
            .collect(),
    )
}

fn compile_let_recursive(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    let_: &ssf::ir::LetRecursive,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::build::TypedExpression, fmm::build::BuildError> {
    let mut variables = variables.clone();
    let mut closure_pointers = HashMap::new();

    for definition in let_.definitions() {
        let closure_pointer =
            instruction_builder.allocate_heap(types::compile_sized_closure(definition, types));

        variables.insert(
            definition.name().into(),
            fmm::build::bit_cast(
                fmm::types::Pointer::new(types::compile_unsized_closure(definition.type_(), types)),
                closure_pointer.clone(),
            )
            .into(),
        );
        closure_pointers.insert(definition.name(), closure_pointer);
    }

    for definition in let_.definitions() {
        instruction_builder.store(
            closures::compile_closure_content(
                entry_functions::compile(module_builder, definition, &variables, types)?,
                definition
                    .environment()
                    .iter()
                    .map(|free_variable| variables[free_variable.name()].clone())
                    .collect(),
            ),
            closure_pointers[definition.name()].clone(),
        );
    }

    compile(
        module_builder,
        instruction_builder,
        let_.expression(),
        &variables,
        &types,
    )
}

fn compile_arithmetic_operation(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    operation: &ssf::ir::ArithmeticOperation,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::ir::ArithmeticOperation, fmm::build::BuildError> {
    let compile = |expression| {
        compile(
            module_builder,
            instruction_builder,
            expression,
            variables,
            types,
        )
    };

    let lhs = compile(operation.lhs())?;
    let rhs = compile(operation.rhs())?;

    Ok(match operation.operator() {
        ssf::ir::ArithmeticOperator::Add => {
            fmm::build::arithmetic_operation(fmm::ir::ArithmeticOperator::Add, lhs, rhs)?
        }
        ssf::ir::ArithmeticOperator::Subtract => {
            fmm::build::arithmetic_operation(fmm::ir::ArithmeticOperator::Subtract, lhs, rhs)?
        }
        ssf::ir::ArithmeticOperator::Multiply => {
            fmm::build::arithmetic_operation(fmm::ir::ArithmeticOperator::Multiply, lhs, rhs)?
        }
        ssf::ir::ArithmeticOperator::Divide => {
            fmm::build::arithmetic_operation(fmm::ir::ArithmeticOperator::Divide, lhs, rhs)?
        }
    })
}

fn compile_comparison_operation(
    module_builder: &fmm::build::ModuleBuilder,
    instruction_builder: &fmm::build::InstructionBuilder,
    operation: &ssf::ir::ComparisonOperation,
    variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, ssf::types::Record>,
) -> Result<fmm::ir::ComparisonOperation, fmm::build::BuildError> {
    let compile = |expression| {
        compile(
            module_builder,
            instruction_builder,
            expression,
            variables,
            types,
        )
    };

    let lhs = compile(operation.lhs())?;
    let rhs = compile(operation.rhs())?;

    fmm::build::comparison_operation(
        match operation.operator() {
            ssf::ir::ComparisonOperator::Equal => fmm::ir::ComparisonOperator::Equal,
            ssf::ir::ComparisonOperator::NotEqual => fmm::ir::ComparisonOperator::NotEqual,
            ssf::ir::ComparisonOperator::GreaterThan => fmm::ir::ComparisonOperator::GreaterThan,
            ssf::ir::ComparisonOperator::GreaterThanOrEqual => {
                fmm::ir::ComparisonOperator::GreaterThanOrEqual
            }
            ssf::ir::ComparisonOperator::LessThan => fmm::ir::ComparisonOperator::LessThan,
            ssf::ir::ComparisonOperator::LessThanOrEqual => {
                fmm::ir::ComparisonOperator::LessThanOrEqual
            }
        },
        lhs,
        rhs,
    )
}

fn compile_primitive(primitive: &ssf::ir::Primitive) -> fmm::ir::Primitive {
    match primitive {
        ssf::ir::Primitive::Boolean(boolean) => fmm::ir::Primitive::Boolean(*boolean),
        ssf::ir::Primitive::Float32(number) => fmm::ir::Primitive::Float32(*number),
        ssf::ir::Primitive::Float64(number) => fmm::ir::Primitive::Float64(*number),
        ssf::ir::Primitive::Integer8(number) => fmm::ir::Primitive::Integer8(*number),
        ssf::ir::Primitive::Integer32(number) => fmm::ir::Primitive::Integer32(*number),
        ssf::ir::Primitive::Integer64(number) => fmm::ir::Primitive::Integer64(*number),
    }
}
