use std::collections::HashMap;

pub const FUNCTION_ARGUMENT_OFFSET: usize = 1;

pub fn get_arity(type_: &fmm::types::Function) -> usize {
    type_.arguments().len() - FUNCTION_ARGUMENT_OFFSET
}

pub fn compile(
    type_: &eir::types::Type,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Type {
    match type_ {
        eir::types::Type::Function(function) => {
            fmm::types::Pointer::new(compile_unsized_closure(function, types)).into()
        }
        eir::types::Type::Primitive(primitive) => compile_primitive(primitive),
        eir::types::Type::Reference(reference) => compile_reference(reference, types),
        eir::types::Type::String => compile_string().into(),
        eir::types::Type::Variant => compile_variant().into(),
    }
}

pub fn compile_primitive(primitive: &eir::types::Primitive) -> fmm::types::Type {
    match primitive {
        eir::types::Primitive::Boolean => fmm::types::Primitive::Boolean.into(),
        eir::types::Primitive::Number => fmm::types::Primitive::Float64.into(),
    }
}

pub fn compile_string() -> fmm::types::Record {
    fmm::types::Record::new(vec![
        fmm::types::Pointer::new(fmm::types::Primitive::Integer8).into(),
        fmm::types::Primitive::PointerInteger.into(),
    ])
}

fn compile_variant() -> fmm::types::Record {
    fmm::types::Record::new(vec![
        compile_variant_tag().into(),
        compile_variant_payload().into(),
    ])
}

pub fn compile_variant_tag() -> fmm::types::Pointer {
    // TODO Add GC functions.
    fmm::types::Pointer::new(fmm::types::Record::new(vec![
        fmm::types::Primitive::Integer8.into(),
    ]))
}

pub fn compile_variant_payload() -> fmm::types::Primitive {
    fmm::types::Primitive::Integer64
}

// TODO Optimize ID representation.
pub fn compile_type_id(type_: &eir::types::Type) -> String {
    format!("{:?}", type_)
}

pub fn compile_reference(
    reference: &eir::types::Reference,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Type {
    compile_record(&types[reference.name()], types)
}

pub fn compile_record(
    record: &eir::types::Record,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Type {
    if is_record_boxed(record) {
        fmm::types::Pointer::new(fmm::types::Record::new(vec![])).into()
    } else {
        compile_unboxed_record(record, types).into()
    }
}

pub fn is_reference_boxed(
    reference: &eir::types::Reference,
    types: &HashMap<String, eir::types::Record>,
) -> bool {
    is_record_boxed(&types[reference.name()])
}

// TODO Unbox small non-recursive records.
pub fn is_record_boxed(record: &eir::types::Record) -> bool {
    !record.elements().is_empty()
}

pub fn compile_unboxed_reference(
    reference: &eir::types::Reference,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Record {
    compile_unboxed_record(&types[reference.name()], types)
}

fn compile_unboxed_record(
    record: &eir::types::Record,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Record {
    fmm::types::Record::new(
        record
            .elements()
            .iter()
            .map(|type_| compile(type_, types))
            .collect(),
    )
}

pub fn compile_sized_closure(
    definition: &eir::ir::Definition,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Record {
    compile_raw_closure(
        compile_entry_function_from_definition(definition, types),
        compile_closure_payload(definition, types),
    )
}

pub fn compile_closure_payload(
    definition: &eir::ir::Definition,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Type {
    if definition.is_thunk() {
        fmm::types::Type::Union(fmm::types::Union::new(
            vec![compile_environment(definition, types).into()]
                .into_iter()
                .chain(vec![compile(definition.result_type(), types)])
                .collect(),
        ))
    } else {
        compile_environment(definition, types).into()
    }
}

pub fn compile_unsized_closure(
    function: &eir::types::Function,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Record {
    compile_raw_closure(
        compile_entry_function(function.arguments(), function.last_result(), types),
        compile_unsized_environment(),
    )
}

pub fn compile_raw_closure(
    entry_function: fmm::types::Function,
    environment: impl Into<fmm::types::Type>,
) -> fmm::types::Record {
    fmm::types::Record::new(vec![
        entry_function.into(),
        compile_arity().into(),
        environment.into(),
    ])
}

pub fn compile_environment(
    definition: &eir::ir::Definition,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Record {
    compile_raw_environment(
        definition
            .environment()
            .iter()
            .map(|argument| compile(argument.type_(), types)),
    )
}

pub fn compile_raw_environment(
    types: impl IntoIterator<Item = fmm::types::Type>,
) -> fmm::types::Record {
    fmm::types::Record::new(types.into_iter().collect())
}

pub fn compile_unsized_environment() -> fmm::types::Record {
    fmm::types::Record::new(vec![])
}

pub fn compile_curried_entry_function(
    function: &fmm::types::Function,
    arity: usize,
) -> fmm::types::Function {
    if arity == get_arity(function) {
        function.clone()
    } else {
        fmm::types::Function::new(
            function.arguments()[..arity + FUNCTION_ARGUMENT_OFFSET].to_vec(),
            fmm::types::Pointer::new(compile_raw_closure(
                fmm::types::Function::new(
                    function.arguments()[..FUNCTION_ARGUMENT_OFFSET]
                        .iter()
                        .chain(function.arguments()[arity + FUNCTION_ARGUMENT_OFFSET..].iter())
                        .cloned()
                        .collect::<Vec<_>>(),
                    function.result().clone(),
                    fmm::types::CallingConvention::Source,
                ),
                compile_unsized_environment(),
            )),
            fmm::types::CallingConvention::Source,
        )
    }
}

pub fn compile_entry_function_from_definition(
    definition: &eir::ir::Definition,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Function {
    compile_entry_function(
        definition
            .arguments()
            .iter()
            .map(|argument| argument.type_()),
        definition.result_type(),
        types,
    )
}

pub fn compile_entry_function<'a>(
    arguments: impl IntoIterator<Item = &'a eir::types::Type>,
    result: &eir::types::Type,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Function {
    fmm::types::Function::new(
        vec![fmm::types::Pointer::new(compile_unsized_environment()).into()]
            .into_iter()
            .chain(arguments.into_iter().map(|type_| compile(type_, types)))
            .collect(),
        compile(result, types),
        fmm::types::CallingConvention::Source,
    )
}

pub fn compile_foreign_function(
    function: &eir::types::Function,
    calling_convention: eir::ir::CallingConvention,
    types: &HashMap<String, eir::types::Record>,
) -> fmm::types::Function {
    fmm::types::Function::new(
        function
            .arguments()
            .into_iter()
            .map(|type_| compile(type_, types))
            .collect(),
        compile(function.last_result(), types),
        compile_calling_convention(calling_convention),
    )
}

fn compile_calling_convention(
    calling_convention: eir::ir::CallingConvention,
) -> fmm::types::CallingConvention {
    match calling_convention {
        eir::ir::CallingConvention::Source => fmm::types::CallingConvention::Source,
        eir::ir::CallingConvention::Target => fmm::types::CallingConvention::Target,
    }
}

pub fn compile_arity() -> fmm::types::Primitive {
    fmm::types::Primitive::PointerInteger
}
