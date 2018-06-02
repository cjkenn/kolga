extern crate snowc;

use std::fs::File;
use std::env;

use snowc::lexer::Lexer;
use snowc::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    // TODO: repl
    if args.len() < 2 {
        println!("Usage: snow [filename]");
        return;
    }


    let filename = &args[1];
    let infile = match File::open(filename) {
        Ok(file) => file,
        Err(e) => {
            println!("snow: could not open file '{}': {:?}", filename, e.kind());
            return;
        }
    };

    let mut lexer = Lexer::new(infile);
    let _parser = Parser::new(&mut lexer);
}
