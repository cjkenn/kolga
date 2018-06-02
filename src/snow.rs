extern crate snowc;

use std::fs::File;

use snowc::lexer::Lexer;
use snowc::token::Token;

fn main() {
    let infile = File::open("testfile").unwrap_or_else(|_| panic!("Cannot find file"));
    let mut lexer = Lexer::new(infile);
    let tkn = lexer.lex().unwrap();
    println!("{:?}", tkn);
    let tkn2 = lexer.lex().unwrap();
    println!("{:?}", tkn2);
}
