extern crate kolgac;
extern crate ty;
extern crate gen;
extern crate error;

use std::fs::File;
use std::env;
use kolgac::lexer::Lexer;
use kolgac::parser::Parser;
use kolgac::symtab::SymbolTable;
use ty::infer::TyInfer;
use ty::check::TyCheck;
use gen::llvm::CodeGenerator;
use gen::valtab::ValTab;
use error::KolgaErr;

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

    let mut symtab = SymbolTable::new();
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

    // The ast is made mutable so that type inference can write to ast nodes with
    // type information later on.
    let mut ast = parse_result.ast.unwrap();

    // Open a new scope for ty inference and checking, because we need
    // a mutable reference to the ast in order to build the types in place.
    // We can be assured that all ast values are Some, since None is only returned
    // if there are parsing errors.
    {
        let _result = TyInfer::new().infer(&mut ast);
        let check_result = TyCheck::new(&ast, &mut symtab).check();
        if check_result.len() > 0 {
            for err in &check_result {
                err.emit();
            }

            return;
        }
    }

    let mut valtab = ValTab::new();
    let mut llvm_codegen = CodeGenerator::new(&ast, &mut valtab);

    llvm_codegen.gen_ir();

    if llvm_codegen.errors.len() > 0 {
        for err in &llvm_codegen.errors {
            err.emit();
        }

        return;
    }

    llvm_codegen.dump_ir();
}
