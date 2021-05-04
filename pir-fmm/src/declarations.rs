use super::types;
use std::collections::HashMap;

pub fn compile_declaration(
    module_builder: &fmm::build::ModuleBuilder,
    declaration: &pir::ir::Declaration,
    types: &HashMap<String, pir::types::RecordBody>,
) {
    module_builder.declare_variable(
        declaration.name(),
        types::compile_unsized_closure(declaration.type_(), types),
    );
}
