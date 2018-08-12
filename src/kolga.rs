extern crate kolgac;
extern crate types;
extern crate llvm_codegen;

use std::fs::File;
use std::env;

use kolgac::lexer::Lexer;
use kolgac::parser::Parser;
use kolgac::symtab::SymTab;

use types::check::TyCheck;

use llvm_codegen::codegen::CodeGenerator;
use llvm_codegen::valtab::ValTab;

fn main() {
    let args: Vec<String> = env::args().collect();
    // TODO: repl
    if args.len() < 2 {
        println!("Usage: kolga [filename]");
        return;
    }

    let filename = &args[1];
    let infile = match File::open(filename) {
        Ok(file) => file,
        Err(e) => {
            println!("kolga: could not open file '{}': {:?}", filename, e.kind());
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

    let ast = parse_result.ast.unwrap();

    // We can be assured that all ast values are Some, since None is only returned
    // if there are parsing errors
    let tyresult = TyCheck::new(&ast, &mut symtab).check();
    if tyresult.len() > 0 {
        for err in &tyresult {
            err.emit();
        }

        return;
    }

    let mut valtab = ValTab::new();
    let mut llvm_codegen = CodeGenerator::new(&ast, &mut symtab, &mut valtab);
    llvm_codegen.gen();
    llvm_codegen.dump_ir();
}
