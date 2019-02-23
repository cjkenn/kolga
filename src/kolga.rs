extern crate error;
extern crate gen;
extern crate kolgac;
extern crate ty;

use error::KolgaErr;
use gen::llvm::CodeGenerator;
use gen::obj::ObjGenerator;
use gen::valtab::ValTab;
use kolgac::lexer::Lexer;
use kolgac::parser::Parser;
use kolgac::symtab::SymbolTable;
use std::env;
use std::fs::File;
use ty::check::TyCheck;
use ty::infer::TyInfer;

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

    // Any errors should already have been emitted by the compiler, whether or
    // not they are continuable. We exit here if there are any errors.
    if parse_result.has_err {
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
        let result = TyInfer::new().infer(&mut ast);
        match result {
            Ok(()) => (),
            Err(e) => {
                e.emit();
                return;
            }
        }

        let check_result = TyCheck::new(&ast, &mut symtab).check();

        if check_result.len() > 0 {
            for err in &check_result {
                err.emit();
            }

            return;
        }
    }

    // Create a new LLVM module and generate IR inside it. The IR can then be passed
    // to the object generator to build machine code.
    let mut valtab = ValTab::new();
    let mut llvm_codegen = CodeGenerator::new(&ast, &mut valtab);

    llvm_codegen.gen_ir();

    if llvm_codegen.errors.len() > 0 {
        for err in &llvm_codegen.errors {
            err.emit();
        }

        return;
    }

    // TODO: create obj files with obj gen to make an executable
    llvm_codegen.dump_ir();

    let prefix = filename.split(".").collect::<Vec<&str>>()[0];
    let obj_filename = format!("{}.{}", prefix, "o");

    let mut obj_gen = ObjGenerator::new(llvm_codegen.module);
    obj_gen.emit(&obj_filename);
}
