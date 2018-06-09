extern crate snowc;
extern crate types;

use std::fs::File;
use std::env;

use snowc::lexer::Lexer;
use snowc::parser::Parser;
use snowc::symtab::SymTab;
use types::check::TyCheck;

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
    let mut symtab = SymTab::new();
    let mut parser = Parser::new(&mut lexer, &mut symtab);

    let result = parser.parse();
    if result.error.len() > 0 {
        for err in &result.error {
            err.emit();
        }

        return;
    }

    // We can be assured that all ast values are Some, since None is only returned
    // if there are parsing errors
    let tychk = TyCheck::new(&result.ast.unwrap()).check();
}
