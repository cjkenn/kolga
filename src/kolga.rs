extern crate error;
extern crate gen;
extern crate kolgac;
extern crate ty;

use error::KolgaErr;
use gen::llvm::CodeGenerator;
use gen::obj::ObjGenerator;
use gen::valtab::ValTab;
use kolgac::ast::Ast;
use kolgac::lexer::Lexer;
use kolgac::parser::{Parser, ParserResult};
use kolgac::symtab::SymbolTable;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use ty::check::TyCheck;
use ty::infer::TyInfer;

#[derive(Default, Debug, Clone)]
struct KolgaOpts {
    pub filename: String,
    pub dump_ast: bool,
    pub dump_ir: bool,
}

impl KolgaOpts {
    pub fn new() -> KolgaOpts {
        KolgaOpts::default()
    }

    pub fn from_args(args: Vec<String>) -> Result<KolgaOpts, String> {
        if args.len() < 2 {
            return Err(String::from("usage: kolga [filename]"));
        }

        let filename = args[1].clone();
        let mut arg_map: HashMap<String, bool> = [
            (String::from("dump-ast"), false),
            (String::from("dump-ir"), false),
        ]
            .iter()
            .cloned()
            .collect();

        for arg in args {
            if arg_map.contains_key(&arg) {
                arg_map.insert(arg, true);
            }
        }

        let opts = KolgaOpts {
            filename: filename,
            dump_ast: *arg_map.get("dump-ast").unwrap(),
            dump_ir: *arg_map.get("dump-ir").unwrap(),
        };

        Ok(opts)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let opts = match KolgaOpts::from_args(args) {
        Ok(opts) => opts,
        Err(error) => {
            println!("{}", error);
            return;
        }
    };

    // 1. Run the lexer/parser.
    let mut symtab = SymbolTable::new();
    let parse_result = run_parser(&opts.filename, &mut symtab);

    // Any errors should already have been emitted by the
    // parser, whether or not they are continuable.
    if parse_result.has_err {
        println!("kolgac: Exiting due to parser errors");
        return;
    }

    // 2. Run the type inferrer and the type checker.
    let mut ast = parse_result.ast.unwrap();
    let ty_result = run_tys(&mut ast, &mut symtab);
    match ty_result {
        Ok(()) => (),
        Err(()) => {
            println!("kolgac: Exiting due to type errors");
            return;
        }
    };

    if opts.dump_ast {
        println!("{:#?}", ast);
    }

    // 3. Run the LLVM IR code generator and the object file creator.
    let gen_result = run_gen(&ast, &opts);
    match gen_result {
        Ok(()) => (),
        Err(()) => {
            println!("kolgac: Exiting due to LLVM IR gen errors");
            return;
        }
    }
}

/// Opens the file from the filename provided, creates a lexer for that file
/// and a parser for that lexer. Fully parses the input file, and returns
/// the result from the parser. This result will contain any errors, as well
/// as the AST from parsing (which will be None if there are errors).
fn run_parser(filename: &str, symtab: &mut SymbolTable) -> ParserResult {
    let infile = match File::open(filename) {
        Ok(file) => file,
        Err(e) => {
            panic!("kolgac: could not open file '{}': {:?}", filename, e.kind());
        }
    };

    let mut lexer = Lexer::new(infile);
    let mut parser = Parser::new(&mut lexer, symtab);
    parser.parse()
}

/// Given a valid AST from parsing, infers any types that were not defined in the
/// source code. After inferring, runs a second pass to check all the types in
/// the AST. Returns an empty result, which can be used as a flag to decide
/// whether to continue to other compilation stages or not.
/// This function also prints any errors encountered during inference/checking.
fn run_tys(ast: &mut Ast, symtab: &mut SymbolTable) -> Result<(), ()> {
    let result = TyInfer::new().infer(ast);
    match result {
        Ok(()) => (),
        Err(e) => {
            e.emit();
            return Err(());
        }
    }

    let check_result = TyCheck::new(ast, symtab).check();

    if check_result.len() > 0 {
        for err in &check_result {
            err.emit();
        }

        return Err(());
    }

    Ok(())
}

/// Given a valid AST with all types inferred and checked, generates LLVM IR
/// from that AST, and then creates an object file from that IR. Like the
/// run_tys() function, this returns an empty result to be used as a flag to decide
/// whether or not to continue with compilation stages. This will print any errors
/// encountered during codegen.
fn run_gen(ast: &Ast, opts: &KolgaOpts) -> Result<(), ()> {
    let mut valtab = ValTab::new();
    let mut llvm_codegen = CodeGenerator::new(&ast, &mut valtab);

    llvm_codegen.gen_ir();

    if llvm_codegen.errors.len() > 0 {
        for err in &llvm_codegen.errors {
            err.emit();
        }

        return Err(());
    }

    if opts.dump_ir {
        llvm_codegen.dump_ir();
    }

    // Generate an object file from LLVM IR
    let prefix = opts.filename.split(".").collect::<Vec<&str>>()[0];
    let obj_filename = format!("{}.{}", prefix, "o");

    let mut obj_gen = ObjGenerator::new(llvm_codegen.module);
    obj_gen.emit(&obj_filename);

    Ok(())
}
