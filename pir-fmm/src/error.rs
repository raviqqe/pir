use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CompileError {
    FmmBuild(fmm::build::BuildError),
    TypeCheck(pir::analysis::TypeCheckError),
}

impl Display for CompileError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for CompileError {}

impl From<fmm::build::BuildError> for CompileError {
    fn from(error: fmm::build::BuildError) -> Self {
        Self::FmmBuild(error)
    }
}

impl From<pir::analysis::TypeCheckError> for CompileError {
    fn from(error: pir::analysis::TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
