#[derive(Debug)]
pub struct ErrReport {
    line: usize,
    pos: usize,
    text: String
}

impl ErrReport {
    pub fn new(li: usize, po: usize, txt: String) -> ErrReport {
        ErrReport {
            line: li,
            pos: po,
            text: txt
        }
    }

    pub fn emit(&self) {
        println!("snow: [{}:{}] {}", self.line, self.pos, self.text);
    }
}
