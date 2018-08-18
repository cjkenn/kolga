extern crate kolgac;

use std::fs::File;

use kolgac::lexer::Lexer;
use kolgac::token::TknTy;
use kolgac::parser::Parser;
use kolgac::ast::Ast;
use kolgac::symtab::SymbolTable;
use kolgac::type_record::TyName;

#[test]
fn parse_empty() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/empty").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    match *ast {
        Ast::Prog{stmts} => {
           assert_eq!(stmts.len(), 0)
        },
        _ => assert!(false, "Expected Ast::Prog, found {:?}", *ast)
    };
}

#[test]
fn parse_var_decl_mutable() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_decl_mutable").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let var_decl = &extract_head(ast)[0];
    match var_decl {
        Ast::VarDecl{ty_rec, ident_tkn, is_imm} => {
            assert_eq!(ty_rec.ty, Some(TyName::Num));
            assert_eq!(*is_imm, false);

            match ident_tkn.ty {
                TknTy::Ident(ref id) => {
                    assert_eq!(id, "x");
                },
                _ => assert!(false, "Expected Ident tkn, found {:?}", ident_tkn.ty)
            }
        },
        _ => assert!(false, "Expected Ast::VarDecl, found {:?}", var_decl)
    };
}

#[test]
fn parse_var_decl_imm() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_decl_imm").unwrap());
    let mut symtab = SymbolTable::new();
    let result = Parser::new(&mut lexer, &mut symtab).parse();
    assert!(result.error.len() >= 1);
}

#[test]
fn parse_var_assign_mutable() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_assign_mutable").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let var_assign = &extract_head(ast)[0];
    match var_assign {
        Ast::VarAssign{ty_rec, ident_tkn, is_imm, value} => {
            assert_eq!(ty_rec.ty, Some(TyName::Num));
            assert_eq!(*is_imm, false);

            match ident_tkn.ty {
                TknTy::Ident(ref id) => {
                    assert_eq!(id, "x");
                },
                _ => expected_tkn("Ident", &ident_tkn.ty)
            }

            let vast = value.clone();
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

#[test]
fn parse_var_assign_imm() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/var_assign_imm").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let var_assign = &extract_head(ast)[0];
    match var_assign {
        Ast::VarAssign{ty_rec, ident_tkn, is_imm, value} => {
            assert_eq!(ty_rec.ty, Some(TyName::Num));
            assert_eq!(*is_imm, true);

            match ident_tkn.ty {
                TknTy::Ident(ref id) => {
                    assert_eq!(id, "x");
                },
                _ => expected_tkn("Ident", &ident_tkn.ty)
            }

            let vast = value.clone();
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

#[test]
fn test_parse_unary_expr() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/unary_expr").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let exprstmt = &extract_head(ast)[0];

    match *exprstmt {
        Ast::ExprStmt(ref unr) => {
            let unr_ast = unr.clone().unwrap();
            match unr_ast {
                Ast::Unary(op_tkn, mb_rhs) => {
                    assert_eq!(op_tkn.ty, TknTy::Minus);
                    let rhs = mb_rhs.clone().unwrap();
                    match rhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &rhs)
                    }
                },
                _ => expected_ast("Unary", &unr_ast)
            }
        },
        _ => expected_ast("ExprStmt", &exprstmt)
    }
}

#[test]
fn test_parse_binary_expr() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/binary_expr").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let exprstmt = &extract_head(ast)[0];

    match *exprstmt {
        Ast::ExprStmt(ref bnr) => {
            let bin_ast = bnr.clone().unwrap();
            match bin_ast {
                Ast::Binary(op_tkn, mb_lhs, mb_rhs) => {
                    assert_eq!(op_tkn.ty, TknTy::Plus);
                    let lhs = mb_lhs.clone().unwrap();
                    let rhs = mb_rhs.clone().unwrap();

                    match lhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &lhs)
                    };

                    match rhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &rhs)
                    };
                },
                _ => expected_ast("Binary", &bin_ast)
            }
        },
        _ => expected_ast("ExprStmt", &exprstmt)
    }
}

#[test]
fn test_logical_expr() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/logical_expr").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let exprstmt = &extract_head(ast)[0];

    match *exprstmt {
        Ast::ExprStmt(ref bnr) => {
            let bin_ast = bnr.clone().unwrap();
            match bin_ast {
                Ast::Logical(op_tkn, mb_lhs, mb_rhs) => {
                    assert_eq!(op_tkn.ty, TknTy::AmpAmp);
                    let lhs = mb_lhs.clone().unwrap();
                    let rhs = mb_rhs.clone().unwrap();

                    match lhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &lhs)
                    };

                    match rhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &rhs)
                    };
                },
                _ => expected_ast("Logical", &bin_ast)
            }
        },
        _ => expected_ast("ExprStmt", &exprstmt)
    }
}

#[test]
fn test_logical_expr_w_keyword() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/logical_keyword_expr").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let exprstmt = &extract_head(ast)[0];

    match *exprstmt {
        Ast::ExprStmt(ref bnr) => {
            let bin_ast = bnr.clone().unwrap();
            match bin_ast {
                Ast::Logical(op_tkn, mb_lhs, mb_rhs) => {
                    assert_eq!(op_tkn.ty, TknTy::And);
                    let lhs = mb_lhs.clone().unwrap();
                    let rhs = mb_rhs.clone().unwrap();

                    match lhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &lhs)
                    };

                    match rhs {
                        Ast::Primary(_) => assert!(true),
                        _ => expected_ast("Primary", &rhs)
                    };
                },
                _ => expected_ast("Logical", &bin_ast)
            }
        },
        _ => expected_ast("ExprStmt", &exprstmt)
    }
}

#[test]
fn test_func_decl() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/func_decl").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let func_decl = &extract_head(ast)[0];

    match func_decl.clone() {
        Ast::FuncDecl{ident_tkn, params, ret_ty, func_body, scope_lvl} => {
            match ident_tkn.ty {
                TknTy::Ident(name) => {
                    assert_eq!(name, "myFn");
                },
                _ => expected_tkn("Ident", &ident_tkn.ty)
            }

            assert_eq!(params.len(), 1);
            assert_eq!(ret_ty.ty.unwrap(), TyName::Num);
            assert_eq!(scope_lvl, 2); // scope level is 2 because the inner block scope is finalized first

            let func_body = func_body.clone().unwrap();

            match func_body {
                Ast::BlckStmt{stmts, scope_lvl} => {
                    assert_eq!(scope_lvl, 1);
                    assert_eq!(stmts.len(), 1);
                },
                _ => expected_ast("BlckStmt", &func_body)
            }

        },
        _ => expected_ast("FnDecl", &func_decl)
    }
}

#[test]
fn test_func_decl_w_call() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/func_decl_w_call").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let func_decl = &extract_head(ast.clone())[0];

    match func_decl.clone() {
        Ast::FuncDecl{ident_tkn, params, ret_ty, func_body, scope_lvl: _} => {
            match ident_tkn.ty {
                TknTy::Ident(name) => {
                    assert_eq!(name, "myFn");
                },
                _ => expected_tkn("Ident", &ident_tkn.ty)
            }

            assert_eq!(params.len(), 1);
            assert_eq!(ret_ty.ty.unwrap(), TyName::Num);

            let func_body = func_body.clone().unwrap();

            match func_body {
                Ast::BlckStmt{stmts, scope_lvl} => {
                    assert_eq!(scope_lvl, 1);
                    assert_eq!(stmts.len(), 1);
                },
                _ => expected_ast("BlckStmt", &func_body)
            }

        },
        _ => expected_ast("FnDecl", &func_decl)
    }

    let func_call_expr = &extract_head(ast)[1];
    match func_call_expr.clone() {
        Ast::ExprStmt(mb_func_ast) => {
            let func_call = mb_func_ast.clone().unwrap();
            match func_call {
                Ast::FnCall(ident_tkn, params) => {
                    match ident_tkn.clone().unwrap().ty {
                        TknTy::Ident(name) => {
                            assert_eq!(name, "myFn");
                        },
                        _ => expected_tkn("Ident", &ident_tkn.unwrap().ty)
                    };

                    assert_eq!(params.len(), 1);
                },
                _ => expected_ast("FnCall", &func_call)
            }
        },
        _ => expected_ast("ExprStmt", &func_call_expr)
    }
}

fn extract_head(ast: Box<Ast>) -> Vec<Ast> {
    match *ast {
        Ast::Prog{stmts} => stmts.clone(),
        _ => panic!("Cannot call extract_head on an ast not of type Ast::Prog")
    }
}

fn expected_ast(expt: &str, found: &Ast) {
    assert!(false, format!("Expected {}, found {:?}", expt, found));
}

fn expected_tkn(expt: &str, found: &TknTy) {
    assert!(false, format!("Expected {}, found {:?}", expt, found));
}
