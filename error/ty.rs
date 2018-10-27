use KolgaErr;

#[derive(Debug, Clone)]
pub enum TypeErrTy {
    TyMismatch(String, String),
    BinOpMismatch(String, String, String, String, String),
    InvalidFn(String),
    InvalidRet(String, String)
}

#[derive(Debug, Clone)]
pub struct TypeErr {
    pub line: usize,
    pub pos: usize,
    pub ty: TypeErrTy
}

impl TypeErr {
    pub fn new(line: usize, pos: usize, ty: TypeErrTy) -> TypeErr {
        TypeErr {
            line: line,
            pos: pos,
            ty: ty
        }
    }
}

impl KolgaErr for TypeErr {
    fn emit(&self) {
        println!("kolgac: Type error - {}", self.to_msg());
    }

    fn to_msg(&self) -> String {
        let str_pos = format!("[Line {}:{}]", self.line, self.pos);

        match self.ty {
            TypeErrTy::TyMismatch(ref expected, ref found) => {
                format!("{} type mismatch: Wanted {} but found {}", str_pos, expected, found)
            },
            TypeErrTy::BinOpMismatch(ref op,
                                  ref expected_frst,
                                  ref expected_scnd,
                                  ref found_frst,
                                  ref found_scnd) => {
                format!("{} operator mismatch: {} wants {} and {}, but found {} and {}",
                        str_pos,
                        op,
                        expected_frst,
                        expected_scnd,
                        found_frst,
                        found_scnd)
            },
            TypeErrTy::InvalidFn(ref found) => {
                format!("{} invalid function name {}", str_pos, found)
            },
            TypeErrTy::InvalidRet(ref found, ref ret_ty) => {
                format!("{} {} expects a return type of {}, but no return found", str_pos, found, ret_ty)
            }
        }
    }
}
