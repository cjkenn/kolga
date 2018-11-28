use KolgaErr;

#[derive(Debug, Clone)]
pub enum ParseErrTy {
    InvalidIdent(String),
    InvalidTkn(String),
    InvalidAssign(String),
    InvalidImmAssign(String),
    InvalidTy(String),
    InvalidForStmt,
    InvalidIfStmt,
    InvalidClassProp,
    ImmDecl(String),
    TknMismatch(String, String), // not continuable
    FnParamCntExceeded(usize), // not continuable
    WrongFnParamCnt(usize, usize), // continuable
    UndeclaredSym(String), // continuable
    UnassignedVar(String) // continuable
}

#[derive(Debug, Clone)]
pub struct ParseErr {
    pub line: usize,
    pub pos: usize,
    pub ty: ParseErrTy
}

impl ParseErr {
    pub fn new(line: usize, pos: usize, ty: ParseErrTy) -> ParseErr {
        ParseErr {
            line: line,
            pos: pos,
            ty: ty
        }
    }

    pub fn continuable(self) -> bool {
        match self.ty {
            ParseErrTy::TknMismatch(_,_) => false,
            ParseErrTy::FnParamCntExceeded(_) => false,
            _ => true
        }
    }
}

impl KolgaErr for ParseErr {
    fn emit(&self) {
        println!("kolgac: Parse error - {}", self.to_msg());
    }

    fn to_msg(&self) -> String {
        let str_pos = format!("[Line {}:{}]", self.line, self.pos);

        match self.ty {
            ParseErrTy::InvalidIdent(ref found) => {
                format!("{} Invalid identifier '{}' found", str_pos, found)
            },
            ParseErrTy::InvalidTkn(ref found) => {
                format!("{} Invalid token '{}' found", str_pos, found)
            },
            ParseErrTy::InvalidAssign(ref found) => {
                format!("{} '{}' is not a valid assignment value", str_pos, found)
            },
            ParseErrTy::InvalidImmAssign(ref found) => {
                format!("{} Cannot re-assign immutable variable '{}'", str_pos, found)
            },
            ParseErrTy::InvalidTy(ref found) => {
                format!("{} '{}' is not a valid type", str_pos, found)
            },
            ParseErrTy::InvalidForStmt => {
                format!("{} Invalid for loop: must start with a variable declaration", str_pos)
            },
            ParseErrTy::InvalidIfStmt => {
                format!("{} Invalid if statement: cannot contain more than one else condition", str_pos)
            },
            ParseErrTy::InvalidClassProp => {
                format!("{} Invalid class property declaration", str_pos)
            },
            ParseErrTy::ImmDecl(ref found) => {
                format!("{} Cannot declare immutable variable '{}' with no value", str_pos, found)
            },
            ParseErrTy::TknMismatch(ref expected, ref found) => {
                format!("{} Expected token '{}', but found '{}'", str_pos, expected, found)
            },
            ParseErrTy::FnParamCntExceeded(ref expected) => {
                format!("{} Parameter count exceeds limit of {}", str_pos, expected)
            },
            ParseErrTy::WrongFnParamCnt(ref expected, ref found) => {
                format!("{} Expected {} parameters, but found {}", str_pos, expected, found)
            },
            ParseErrTy::UnassignedVar(ref found) => {
                format!("{} Cannot reference un-assigned variable '{}'", str_pos, found)
            },
            ParseErrTy::UndeclaredSym(ref found) => {
                format!("{} Undeclared symbol '{}' found", str_pos, found)
            }
        }
    }
}
