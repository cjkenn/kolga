#[derive(Debug,Clone)]
pub struct ErrC {
    line: usize,
    pos: usize,
    text: String
}

impl ErrC {
    pub fn new(li: usize, po: usize, txt: String) -> ErrC {
        ErrC {
            line: li,
            pos: po,
            text: txt
        }
    }

    pub fn new_ty(li: usize, po: usize, exct_ty: &str, found_ty: &str) -> ErrC {
        ErrC {
            line: li,
            pos: po,
            text: format!("Type mismatch: Expected {}, but found {}", exct_ty, found_ty)
        }
    }

    pub fn emit(&self) {
        println!("snow: [{}:{}] {}", self.line, self.pos, self.text);
    }
}
