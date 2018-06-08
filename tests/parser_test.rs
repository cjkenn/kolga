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
