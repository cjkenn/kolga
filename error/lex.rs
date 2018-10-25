use KolgaErr;

#[derive(Debug, Clone)]
pub enum LexErrTy {
    UnknownChar(char),
    UnterminatedStr(String)
}

pub struct LexErr {
    pub line: usize,
    pub pos: usize,
    pub ty: LexErrTy
}

impl LexErr {
    pub fn new(line: usize, pos: usize, ty: LexErrTy) -> LexErr {
        LexErr {
            line: line,
            pos: pos,
            ty: ty
        }
    }
}

impl KolgaErr for LexErr {
    fn emit(&self) {
        println!("kolgac: Parse error - {}", self.to_msg());
    }

    fn to_msg(&self) -> String {
        let str_pos = format!("[Line {}:{}]", self.line, self.pos);

        match self.ty {
            LexErrTy::UnknownChar(ref ch) => {
                format!("{} Unrecognized character '{}'", str_pos, ch)
            },
            LexErrTy::UnterminatedStr(ref found) => {
                format!("{} Unterminated string literal '{}'", str_pos, found)
            }
        }
    }
}
