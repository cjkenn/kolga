extern crate kolgac;

use std::fs::File;

use kolgac::lexer::Lexer;
use kolgac::token::TknTy;
use kolgac::parser::Parser;
use kolgac::ast::Ast;
use kolgac::symtab::SymbolTable;
use kolgac::ty_rec::TyName;

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
        Ast::VarDecl{ty_rec, ident_tkn, is_imm, is_global} => {
            assert_eq!(ty_rec.ty, Some(TyName::Num));
            assert_eq!(*is_imm, false);
            assert_eq!(*is_global, true);

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
        Ast::VarAssign{ty_rec, ident_tkn, is_imm, is_global, value} => {
            assert_eq!(ty_rec.ty, Some(TyName::Num));
            assert_eq!(*is_imm, false);
            assert_eq!(*is_global, true);

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
        Ast::VarAssign{ty_rec, ident_tkn, is_imm, is_global, value} => {
            assert_eq!(ty_rec.ty, Some(TyName::Num));
            assert_eq!(*is_imm, true);
            assert_eq!(*is_global, true);

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
fn parse_unary_expr() {
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
fn parse_binary_expr() {
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
fn parse_logical_expr() {
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
fn parse_logical_expr_w_keyword() {
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
fn parse_fn_decl() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/fn_decl").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let fn_decl = &extract_head(ast)[0];

    match fn_decl.clone() {
        Ast::FnDecl{ident_tkn, fn_params, ret_ty, fn_body, sc} => {
            match ident_tkn.ty {
                TknTy::Ident(name) => {
                    assert_eq!(name, "myFn");
                },
                _ => expected_tkn("Ident", &ident_tkn.ty)
            }

            assert_eq!(fn_params.len(), 1);
            assert_eq!(ret_ty.ty.unwrap(), TyName::Num);
            assert_eq!(sc, 1);

            let fn_body = fn_body.clone().unwrap();

            match fn_body {
                Ast::BlckStmt{stmts, sc} => {
                    assert_eq!(sc, 1);
                    assert_eq!(stmts.len(), 1);
                },
                _ => expected_ast("BlckStmt", &fn_body)
            }

        },
        _ => expected_ast("FnDecl", &fn_decl)
    }
}

#[test]
fn parse_fn_decl_w_call() {
    let mut lexer = Lexer::new(File::open("./tests/parser_input/fn_decl_w_call").unwrap());
    let mut symtab = SymbolTable::new();
    let ast = Parser::new(&mut lexer, &mut symtab).parse().ast.unwrap();

    let fn_decl = &extract_head(ast.clone())[0];

    match fn_decl.clone() {
        Ast::FnDecl{ident_tkn, fn_params, ret_ty, fn_body, sc: _} => {
            match ident_tkn.ty {
                TknTy::Ident(name) => {
                    assert_eq!(name, "myFn");
                },
                _ => expected_tkn("Ident", &ident_tkn.ty)
            }

            assert_eq!(fn_params.len(), 1);
            assert_eq!(ret_ty.ty.unwrap(), TyName::Num);

            let fn_body = fn_body.clone().unwrap();

            match fn_body {
                Ast::BlckStmt{stmts, sc} => {
                    assert_eq!(sc, 1);
                    assert_eq!(stmts.len(), 1);
                },
                _ => expected_ast("BlckStmt", &fn_body)
            }

        },
        _ => expected_ast("FnDecl", &fn_decl)
    }

    let fn_call_expr = &extract_head(ast)[1];
    match fn_call_expr.clone() {
        Ast::ExprStmt(mb_func_ast) => {
            let fn_call = mb_func_ast.clone().unwrap();
            match fn_call {
                Ast::FnCall{fn_tkn, fn_params} => {
                    match fn_tkn.clone().ty {
                        TknTy::Ident(name) => {
                            assert_eq!(name, "myFn");
                        },
                        _ => expected_tkn("Ident", &fn_tkn.ty)
                    };

                    assert_eq!(fn_params.len(), 1);
                },
                _ => expected_ast("FnCall", &fn_call)
            }
        },
        _ => expected_ast("ExprStmt", &fn_call_expr)
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
