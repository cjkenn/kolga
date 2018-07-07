extern crate snowc;
extern crate types;
extern crate vm;

use std::fs::File;
use std::env;

use snowc::lexer::Lexer;
use snowc::parser::Parser;
use snowc::symtab::SymTab;
use types::check::TyCheck;
use vm::vm::Vm;
use vm::reg::Reg;
use vm::op::OpCode;

fn main() {
    let mut vm = Vm::new();
    let dest = Reg::new(String::from("r0"));
    let op1 = Reg::new(String::from("r1"));
    let op2 = Reg::with_val(String::from("r2"), 5.0);

    let mut code = OpCode::Add(dest, op1, op2);
    let _res = vm.execute(&mut code);

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

    let mut symtab = SymTab::new();
    let parse_result;

    // New scope for this so the borrow of the symbol table
    // expires so we can use it in the type checker. We don't care if the
    // lexer or parser go out of scope either, since we only need the ast
    // in the result later on
    {
        let mut lexer = Lexer::new(infile);
        let mut parser = Parser::new(&mut lexer, &mut symtab);
        parse_result = parser.parse();

    }

    if parse_result.error.len() > 0 {
        for err in &parse_result.error {
            err.emit();
        }

        return;
    }

    // We can be assured that all ast values are Some, since None is only returned
    // if there are parsing errors
    let tyresult = TyCheck::new(&parse_result.ast.unwrap(), &mut symtab).check();
    if tyresult.len() > 0 {
        for err in &tyresult {
            err.emit();
        }

        return;
    }
}
