use crate::KolgaErr;

#[derive(Debug, Clone)]
pub enum GenErrTy {
    InvalidAst,
    InvalidFn(String),
    InvalidFnParam,
    InvalidClass(String),
}

pub struct GenErr {
    pub ty: GenErrTy,
}

impl GenErr {
    pub fn new(ty: GenErrTy) -> GenErr {
        GenErr { ty: ty }
    }
}

impl KolgaErr for GenErr {
    fn emit(&self) {
        println!("kolgac: Failed to generate LLVM IR - {}", self.to_msg());
    }

    fn to_msg(&self) -> String {
        match self.ty {
            GenErrTy::InvalidAst => format!("Code generation failed for provided AST"),
            GenErrTy::InvalidFn(ref found) => format!("'{}' is not a valid function", found),
            GenErrTy::InvalidFnParam => format!("Invalid function parameter"),
            GenErrTy::InvalidClass(ref found) => format!("'{}' is not a valid class", found),
        }
    }
}
