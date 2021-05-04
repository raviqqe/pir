use crate::{entry_functions, expressions, types};
use std::collections::HashMap;

pub fn compile_definition(
    module_builder: &fmm::build::ModuleBuilder,
    definition: &pir::ir::Definition,
    global_variables: &HashMap<String, fmm::build::TypedExpression>,
    types: &HashMap<String, pir::types::RecordBody>,
) -> Result<(), fmm::build::BuildError> {
    module_builder.define_variable(
        definition.name(),
        fmm::build::record(vec![
            entry_functions::compile(module_builder, definition, global_variables, types)?,
            expressions::compile_arity(definition.arguments().iter().count()).into(),
            fmm::ir::Undefined::new(types::compile_closure_payload(definition, types)).into(),
        ]),
        definition.is_thunk(),
        fmm::ir::Linkage::External,
    );

    Ok(())
}
