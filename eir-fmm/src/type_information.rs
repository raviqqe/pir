use crate::types;

pub fn compile_type_information_global_variable(
    module_builder: &fmm::build::ModuleBuilder,
    type_: &eir::types::Type,
) -> Result<(), fmm::build::BuildError> {
    // TODO Define GC functions.
    module_builder.define_variable(
        types::compile_type_id(type_),
        fmm::build::record(vec![fmm::ir::Primitive::Integer8(0).into()]),
        false,
        fmm::ir::Linkage::Weak,
    );

    Ok(())
}
