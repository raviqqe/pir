use super::types;
use std::collections::HashMap;

pub fn compile_declaration(
    module_builder: &fmm::build::ModuleBuilder,
    declaration: &eir::ir::Declaration,
    types: &HashMap<String, eir::types::Record>,
) {
    module_builder.declare_variable(
        declaration.name(),
        types::compile_unsized_closure(declaration.type_(), types),
    );
}
