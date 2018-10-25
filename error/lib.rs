#[derive(Debug, Clone)]
pub struct ErrC {
    pub line: usize,
    pub pos: usize,
    pub text: String
}

#[derive(Debug, Clone)]
pub struct ParseErr {
    line: usize,
    pos: usize,
    ty: ParseErrTy
}

#[derive(Debug, Clone)]
pub enum ParseErrTy {
    InvalidIdent(String),
    InvalidTkn(String),
    InvalidAssign(String),
    InvalidImmAssign(String),
    InvalidTy(String),
    InvalidForStmt,
    ImmDecl(String),
    TknMismatch(String, String),
    FnParamCntExceeded(usize),
    WrongFnParamCnt(usize, usize),
    UndeclaredSym(String),
    UnassignedVar(String)
}

impl ParseErr {
    pub fn new(line: usize, pos: usize, ty: ParseErrTy) -> ParseErr {
        ParseErr {
            line: line,
            pos: pos,
            ty: ty
        }
    }

    pub fn emit(&self) {
        println!("kolgac: Parse error - {}", self.to_msg());
    }

    fn to_msg(&self) -> String {
        let str_pos = format!("[Line {}:{}]", self.line, self.pos);

        match self.ty {
            ParseErrTy::TknMismatch(ref expected, ref found) => {
                format!("{} Expected token '{}', but found '{}'", str_pos, expected, found)
            },
            ParseErrTy::InvalidIdent(ref found) => {
                format!("{} Invalid identifier '{}' found", str_pos, found)
            },
            ParseErrTy::InvalidTkn(ref found) => {
                format!("{} Invalid token '{}' found", str_pos, found)
            },
            ParseErrTy::UnassignedVar(ref found) => {
                format!("{} Cannot reference un-assigned variable '{}'", str_pos, found)
            },
            ParseErrTy::UndeclaredSym(ref found) => {
                format!("{} Undeclared symbol '{}' found", str_pos, found)
            },
            ParseErrTy::WrongFnParamCnt(ref expected, ref found) => {
                format!("{} Expected {} parameters, but found {}", str_pos, expected, found)
            },
            ParseErrTy::FnParamCntExceeded(ref expected) => {
                format!("{} Parameter count exceeds limit of {}", str_pos, expected)
            },
            ParseErrTy::InvalidAssign(ref found) => {
                format!("{} '{}' is not a valid assignment value", str_pos, found)
            },
            ParseErrTy::InvalidImmAssign(ref found) => {
                format!("{} Cannot re-assign immutable variable '{}'", str_pos, found)
            },
            ParseErrTy::InvalidForStmt => {
                format!("{} Invalid for loop: must start with a variable declaration", str_pos)
            },
            ParseErrTy::InvalidTy(ref found) => {
                format!("{} '{}' is not a valid type", str_pos, found)
            },
            ParseErrTy::ImmDecl(ref found) => {
                format!("{} Cannot declare immutable variable '{}' with no value", str_pos, found)
            }
        }
    }
}

impl ErrC {
    pub fn new(li: usize, po: usize, txt: String) -> ErrC {
        ErrC {
            line: li,
            pos: po,
            text: txt
        }
    }

    pub fn emit(&self) {
        println!("kolgac: [Line {}:{}] {}", self.line, self.pos, self.text);
    }
}

#[derive(Debug, Clone)]
pub struct ErrCodeGen {
    text: String
}

impl ErrCodeGen {
    pub fn new(txt: String) -> ErrCodeGen {
        ErrCodeGen {
            text: txt
        }
    }

    pub fn emit(&self) {
        println!("kolga: Failed to generate LLVM ir: {}", self.text)
    }
}

#[derive(Debug, Clone)]
pub struct ErrRuntime {
    text: String
}

impl ErrRuntime {
    pub fn new(txt: String) -> ErrRuntime {
        ErrRuntime {
            text: txt
        }
    }

    pub fn emit(&self) {
        println!("kolga: Runtime Error -> {}", self.text)
    }
}
