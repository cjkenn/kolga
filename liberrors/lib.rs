#[derive(Debug,Clone)]
pub struct ErrC {
    line: usize,
    pos: usize,
    text: String
}

#[derive(Debug,Clone)]
pub struct ErrRuntime {
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

    pub fn emit(&self) {
        println!("snow: [Line {}:{}] {}", self.line, self.pos, self.text);
    }
}

impl ErrRuntime {
    pub fn new(txt: String) -> ErrRuntime {
        ErrRuntime {
            text: txt
        }
    }

    pub fn emit(&self) {
        println!("snow: Runtime Error -> {}", self.text)
    }
}
