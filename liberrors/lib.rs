#[derive(Debug, Clone)]
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

    pub fn emit(&self) {
        println!("kolga: [Line {}:{}] {}", self.line, self.pos, self.text);
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
