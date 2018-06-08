extern crate snowc;

use std::fs::File;

use snowc::lexer::Lexer;
use snowc::token::TknTy;
use snowc::parser::Parser;
use snowc::ast::Ast;
use snowc::symtab::SymTab;

#[test]
fn test_parse_empty() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/empty").unwrap());
    let mut symtab = SymTab::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    match *ast {
        Ast::Prog(stmts) => {
           assert_eq!(stmts.len(), 0)
        },
        _ => assert!(false, "Expected Ast::Prog, found {:?}", *ast)
    };
}

#[test]
fn test_parse_var_decl_mutable() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_decl_mutable").unwrap());
    let mut symtab = SymTab::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap().extract_head();

    let var_decl = &ast[0];
    match *var_decl {
        Ast::VarDecl(ref varty, ref ident, imm) => {
            assert_eq!(varty.ty, TknTy::Num);
            assert_eq!(imm, false);

            match ident.ty {
                TknTy::Ident(ref id) => {
                    assert_eq!(id, "x");
                },
                _ => assert!(false, "Expected Ident tkn, found {:?}", ident.ty)
            }
        },
        _ => assert!(false, "Expected Ast::VarDecl, found {:?}", var_decl)
    };
}

#[test]
fn test_parse_var_decl_imm() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_decl_imm").unwrap());
    let mut symtab = SymTab::new();
    let result = Parser::new(&mut lexer, &mut symtab).parse();
    assert!(result.error.len() >= 1);
}

#[test]
fn test_parse_var_assign_mutable() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_assign_mutable").unwrap());
    let mut symtab = SymTab::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap().extract_head();

    let var_assign = &ast[0];
    match *var_assign {
        Ast::VarAssign(ref varty, ref ident, imm, ref val_ast) => {
            assert_eq!(varty.ty, TknTy::Num);
            assert_eq!(imm, false);

            match ident.ty {
                TknTy::Ident(ref id) => {
                    assert_eq!(id, "x");
                },
                _ => expected_tkn("Ident", &ident.ty)
            }

            let vast = val_ast.clone();
            match *vast {
                Some(ast) => {
                    match ast {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &ast)
                    }
                },
                _ => ()
            }
        },
        _ => expected_ast("VarAssign", &var_assign)
    }
}

fn expected_ast(expt: &str, found: &Ast) {
    assert!(false, format!("Expected {}, found {:?}", expt, found));
}

fn expected_tkn(expt: &str, found: &TknTy) {
    assert!(false, format!("Expected {}, found {:?}", expt, found));
}
