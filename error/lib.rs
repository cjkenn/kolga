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
    TknMismatch(String, String),
    InvalidIdent(String),
    InvalidToken(String),
    InvalidAssign(String),
    InvalidType(String),
    UndeclaredSym(String),
    UnassignedVar(String),
    WrongFnParamCnt(usize),
    FnParamCntExceeded(usize),
    InvalidImmAssign(String),
    InvalidForStmt(String),
    ImmDecl(String)
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
        match self.ty {
            ParseErrTy::TknMismatch(ref expected, ref found) => {
                format!("[Line {}:{}] Expected token '{}', but found '{}'",
                        self.line,
                        self.pos,
                        expected,
                        found)
            },
            _ => panic!("Illegal error type found!")
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
