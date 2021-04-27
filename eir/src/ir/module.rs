use super::declaration::Declaration;
use super::definition::Definition;
use super::foreign_declaration::ForeignDeclaration;
use super::foreign_definition::ForeignDefinition;
use super::type_definition::TypeDefinition;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    type_definitions: Vec<TypeDefinition>,
    foreign_declarations: Vec<ForeignDeclaration>,
    foreign_definitions: Vec<ForeignDefinition>,
    declarations: Vec<Declaration>,
    definitions: Vec<Definition>,
}

impl Module {
    pub fn new(
        type_definitions: Vec<TypeDefinition>,
        foreign_declarations: Vec<ForeignDeclaration>,
        foreign_definitions: Vec<ForeignDefinition>,
        declarations: Vec<Declaration>,
        definitions: Vec<Definition>,
    ) -> Self {
        Self {
            type_definitions,
            foreign_declarations,
            foreign_definitions,
            declarations,
            definitions: definitions
                .iter()
                .map(|definition| definition.infer_environment(&Default::default()))
                .collect(),
        }
    }

    pub fn type_definitions(&self) -> &[TypeDefinition] {
        &self.type_definitions
    }

    pub fn foreign_declarations(&self) -> &[ForeignDeclaration] {
        &self.foreign_declarations
    }

    pub fn foreign_definitions(&self) -> &[ForeignDefinition] {
        &self.foreign_definitions
    }

    pub fn declarations(&self) -> &[Declaration] {
        &self.declarations
    }

    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }
}
