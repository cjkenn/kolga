#[derive(Debug)]
pub struct Report {
    line: usize,
    pos: usize,
    text: String
}

impl Report {
    pub fn new(li: usize, po: usize, txt: String) -> Report {
        Report {
            line: li,
            pos: po,
            text: txt
        }
    }

    pub fn emit(&self) {
        println!("snow: [{}:{}] {}", self.line, self.pos, self.text);
    }
}
