extern crate clap;

extern crate kolgac_errors;
extern crate kolgac_ir;
extern crate kolgac_llvm;
extern crate kolgac_types;

use clap::Clap;

use kolgac::{
    ast::Ast,
    lexer::Lexer,
    parser::{Parser, ParserResult},
    symtab::SymbolTable,
};
use kolgac_errors::KolgaErr;
use kolgac_ir::irgen::IRGen;
use kolgac_llvm::{llvm::CodeGenerator, obj::ObjGenerator, valtab::ValTab};
use kolgac_types::{check::TyCheck, infer::TyInfer};

use std::fs::File;

#[derive(Clap)]
#[clap(version = "1.0")]
pub struct KolgaOpts {
    filename: String,

    #[clap(long)]
    use_llvm: bool,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_llvm_ir: bool,
    #[clap(long)]
    show_kir: bool,
}

fn main() {
    let opts: KolgaOpts = KolgaOpts::parse();

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

    if opts.show_ast {
        println!("{:#?}", ast);
    }

    // 3. Choose backend from options and generate appropriate code.
    if opts.use_llvm {
        // Using LLVM will create an object file containing bytecode.
        let llvm_result = run_llvm_codegen(&ast, &opts);
        match llvm_result {
            Ok(()) => (),
            Err(()) => {
                println!("kolgac: Exiting due to LLVM IR errors");
                return;
            }
        }
    } else {
        // If not using LLVM, we generate KIR and can perform
        // analysis on it before generating native code.
        let kir_result = run_kir_codegen(&ast, &opts);
        match kir_result {
            Ok(()) => (),
            Err(()) => {
                println!("kolgac: Exiting due to KolIR errors");
                return;
            }
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
fn run_llvm_codegen(ast: &Ast, opts: &KolgaOpts) -> Result<(), ()> {
    let mut valtab = ValTab::new();
    let mut llvm_codegen = CodeGenerator::new(&ast, &mut valtab);

    llvm_codegen.gen_ir();

    if llvm_codegen.errors.len() > 0 {
        for err in &llvm_codegen.errors {
            err.emit();
        }

        return Err(());
    }

    if opts.show_llvm_ir {
        llvm_codegen.dump_ir();
    }

    // Generate an object file from LLVM IR
    let prefix = opts.filename.split(".").collect::<Vec<&str>>()[0];
    let obj_filename = format!("{}.{}", prefix, "o");

    let mut obj_gen = ObjGenerator::new(llvm_codegen.module);
    obj_gen.emit(&obj_filename);

    Ok(())
}

fn run_kir_codegen(ast: &Ast, opts: &KolgaOpts) -> Result<(), ()> {
    let mut kir = IRGen::new(ast);
    kir.gen();

    if opts.show_kir {
        for instr in kir.ir {
            println!("{}", instr);
        }
    }

    Ok(())
}
