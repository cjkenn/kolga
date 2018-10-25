pub mod lex;
pub mod parse;
pub mod gen;

pub trait KolgaErr {
    fn emit(&self);
    fn to_msg(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct ErrC {
    pub line: usize,
    pub pos: usize,
    pub text: String
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
