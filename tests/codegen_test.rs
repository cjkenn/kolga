extern crate llvm_codegen;
extern crate kolgac;

use std::fs::File;
use std::fs;
use std::io::prelude::*;

use llvm_codegen::codegen::CodeGenerator;
use llvm_codegen::valtab::ValTab;

use kolgac::lexer::Lexer;
use kolgac::parser::Parser;
use kolgac::symtab::SymTab;

fn run(input_filename: &str, output_filename: &str, expected_filename: &str) {
    let mut lexer = Lexer::new(File::open(input_filename).unwrap());
    let mut symtab = SymTab::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let mut valtab = ValTab::new();
    let mut codegen = CodeGenerator::new(&ast, &mut symtab, &mut valtab);
    codegen.gen();
    codegen.print_ir(String::from(output_filename));

    diff_files(String::from(output_filename), String::from(expected_filename));
    fs::remove_file(output_filename).ok();
}

fn diff_files(filename1: String, filename2: String) {
    let mut file1 = String::new();
    File::open(filename1).unwrap().read_to_string(&mut file1).ok();
    let lines1: Vec<&str> = file1.split('\n').collect();

    let mut file2 = String::new();
    File::open(filename2).unwrap().read_to_string(&mut file2).ok();
    let lines2: Vec<&str> = file2.split('\n').collect();
    println!("{:?}", lines1);
    println!("{:?}", lines2);
    assert_eq!(lines1.len(), lines2.len());

    for (idx, line1) in lines1.iter().enumerate() {
        let line2 = &lines2[idx];
        assert!(line1 == line2, "Line [{}]: Expected {:?}, but found {:?}", idx, line1, line2);
    }
}

#[test]
fn empty_fn_decl() {
    run("./tests/codegen_input/empty_fn",
        "./tests/codegen_output_empty_fn",
        "./tests/codegen_expected/empty_fn");
}

#[test]
fn fn_call() {
    run("./tests/codegen_input/fn_call",
        "./tests/codegen_output_fn_call",
        "./tests/codegen_expected/fn_call");
}

#[test]
fn if_stmt() {
    run("./tests/codegen_input/if_stmt",
        "./tests/codegen_output_if_stmt",
        "./tests/codegen_expected/if_stmt");
}

#[test]
fn if_else_stmt() {
    run("./tests/codegen_input/if_else_stmt",
        "./tests/codegen_output_if_else_stmt",
        "./tests/codegen_expected/if_else_stmt");
}

#[test]
fn if_elif_stmt() {
    run("./tests/codegen_input/if_elif_stmt",
        "./tests/codegen_output_if_elif_stmt",
        "./tests/codegen_expected/if_elif_stmt");
}

#[test]
fn if_elif_else_stmt() {
    run("./tests/codegen_input/if_elif_else_stmt",
        "./tests/codegen_output_if_elif_else_stmt",
        "./tests/codegen_expected/if_elif_else_stmt");
}

#[test]
fn nested_if_stmt() {
    run("./tests/codegen_input/nested_if_stmt",
        "./tests/codegen_output_nested_if_stmt",
        "./tests/codegen_expected/nested_if_stmt");
}
