use std::rc::Rc;

use kolgac::ast::Ast;
use kolgac::token::{Token, TknTy};
use kolgac::symtab::SymbolTable;
use kolgac::ty_rec::TyName;
use kolgac::sym::Sym;
use error::ty::{TypeErrTy, TypeErr};

pub struct TyCheck<'t, 's> {
    ast: &'t Ast,
    symtab: &'s mut SymbolTable,
    errors: Vec<TypeErr>
}

impl<'t, 's> TyCheck<'t, 's> {
    pub fn new(ast: &'t Ast, symtab: &'s mut SymbolTable) -> TyCheck<'t, 's> {
        TyCheck {
            ast: ast,
            symtab: symtab,
            errors: Vec::new()
        }
    }

    /// Initial entry point into the type checker. Loops through each statement in the
    /// AST and type checks them. Returns a vector of errors encountered during type
    /// checking.
    pub fn check(&mut self) -> Vec<TypeErr> {
        match self.ast {
            Ast::Prog{stmts} => {
                for stmt in stmts {
                    // Pass in 0 for the global scope.
                    self.check_stmt(stmt.clone(), 0);
                }
            },
            _ => ()
        }

        self.errors.clone()
    }

    /// Type checks a statement. This is a recursive function that continually walks the AST
    /// until a leaf node. Takes in a statement AST and a scope level to look up symbols
    /// in the symbol table. We expect the symbol table to be finalized at this point,
    /// since parsing should be complete. Thus, the final_sc parameter represents the index
    /// into the finalized_scopes vector in the symbol table.
    fn check_stmt(&mut self, stmt: Ast, final_sc: usize) {
        match stmt {
            // ignore var declaration without assign (nothing to check)
            Ast::VarDecl{ty_rec:_, ident_tkn:_, is_imm:_, is_global:_} => (),
            Ast::VarAssign{ty_rec:_, ident_tkn:_, is_imm:_, is_global:_, value:_} => {
                self.check_var_assign(stmt, final_sc);
            },
            Ast::ExprStmt(ref maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                match ast {
                    Ast::FnCall{fn_tkn:_, fn_params:_} => {
                        self.check_fn_params(ast, final_sc);
                        ()
                    },
                    Ast::ClassFnCall{class_tkn:_, class_name:_, fn_tkn:_, fn_params:_, sc} => {
                        self.check_fn_params(ast, sc);
                        ()
                    },
                    _ => {
                        self.check_expr(ast, final_sc);
                        ()
                    }
                };
            },
            Ast::IfStmt(expr_ast, maybe_stmt_ast, elif_stmts, maybe_el_stmts) => {
                self.check_expr(expr_ast.clone().unwrap(), final_sc);
                self.check_stmt(maybe_stmt_ast.clone().unwrap(), final_sc);

                for stmt in &elif_stmts {
                    self.check_stmt(stmt.clone().unwrap(), final_sc);
                }

                if maybe_el_stmts.is_some() {
                    self.check_stmt(maybe_el_stmts.clone().unwrap(), final_sc);
                }
            },
            Ast::WhileStmt(maybe_expr_ast, maybe_stmts) => {
                self.check_expr(maybe_expr_ast.clone().unwrap(), final_sc);
                self.check_stmt(maybe_stmts.clone().unwrap(), final_sc);
            },
            Ast::ElifStmt(maybe_expr_ast, maybe_stmt_ast) => {
                self.check_expr(maybe_expr_ast.clone().unwrap(), final_sc);
                self.check_stmt(maybe_stmt_ast.clone().unwrap(), final_sc);
            },
            Ast::ForStmt{for_var_decl, for_cond_expr, for_step_expr, stmts} => {
                if for_var_decl.is_some() {
                    self.check_stmt(for_var_decl.clone().unwrap(), final_sc);
                }

                if for_cond_expr.is_some() {
                    self.check_stmt(for_cond_expr.clone().unwrap(), final_sc);
                }

                if for_step_expr.is_some() {
                    self.check_stmt(for_step_expr.clone().unwrap(), final_sc);
                }

                self.check_stmt(stmts.clone().unwrap(), final_sc);
            },
            Ast::BlckStmt{stmts, sc} => {
                for stmt in &stmts {
                    self.check_stmt(stmt.clone().unwrap(), sc);
                }
            },
            Ast::FnDecl{ident_tkn, fn_params: _, ret_ty, fn_body, sc} => {
                let fn_ty = ret_ty.ty.unwrap();
                let fn_stmts = fn_body.unwrap();
                match fn_stmts {
                    Ast::BlckStmt{stmts, sc: inner_sc} => {
                        self.check_fn_stmts(&ident_tkn, fn_ty, stmts, inner_sc);
                    },
                    _ => self.check_stmt(fn_stmts.clone(), sc)
                };
            },
            Ast::ClassDecl{ident_tkn:_, methods, props, sc} => {
                for prop_stmt in props {
                    self.check_stmt(prop_stmt.clone().unwrap(), sc);
                }

                for stmt in &methods {
                    self.check_stmt(stmt.clone().unwrap(), sc);
                }
            },
            _ => {
                panic!("{:?} Unrecognized statement type found!", stmt)
            }
        }
    }

    fn check_fn_stmts(&mut self, fn_tkn: &Token, fn_ret_ty: TyName, stmts: Vec<Option<Ast>>, sc_lvl: usize) {
        let mut has_ret_stmt = false;

        for maybe_stmt in &stmts {
            let stmt = maybe_stmt.clone().unwrap();
            match stmt {
                Ast::RetStmt(maybe_expr) => {
                    has_ret_stmt = true;
                    if maybe_expr.is_none() {
                        if fn_ret_ty != TyName::Void {
                            self.ty_mismatch(&fn_tkn, &fn_ret_ty, &TyName::Void);
                        }
                    } else {
                        let rhs_ty = self.check_expr(maybe_expr.clone().unwrap(), sc_lvl);
                        if fn_ret_ty != rhs_ty {
                            self.ty_mismatch(&fn_tkn, &fn_ret_ty, &rhs_ty);
                        }
                    }
                },
                _ => self.check_stmt(stmt, sc_lvl)
            };
        }

        if fn_ret_ty != TyName::Void && !has_ret_stmt {
            self.error(fn_tkn.line,
                       fn_tkn.pos,
                       TypeErrTy::InvalidRet(fn_tkn.get_name(), fn_ret_ty.to_string()));
        }
    }

    fn check_expr(&mut self, expr: Ast, final_sc: usize) -> TyName {
        match expr {
            Ast::VarAssign{ty_rec:_, ident_tkn:_, is_imm:_, is_global:_, value:_} => {
                self.check_var_assign(expr, final_sc)
            },
            Ast::Unary(op_tkn, maybe_rhs) => {
                let rhs = maybe_rhs.unwrap();

                if rhs.is_primary() {
                    let rhs_ty_rec = rhs.extract_primary_ty_rec();
                    return self.reduce_unary_ty(op_tkn.clone(), rhs_ty_rec.ty.unwrap());
                } else {
                    let rhs_ty = self.check_expr(rhs, final_sc);
                    return self.reduce_unary_ty(op_tkn.clone(), rhs_ty);
                }
            },
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) |
            Ast::Logical(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs = maybe_lhs.unwrap();
                let rhs = maybe_rhs.unwrap();
                let lhs_ty_name = self.check_expr(lhs, final_sc);
                let rhs_ty_name = self.check_expr(rhs, final_sc);

                self.reduce_bin_ty(op_tkn.clone(), lhs_ty_name, rhs_ty_name)
            },
            Ast::Primary(prim_ty_rec) => {
                match prim_ty_rec.tkn.ty {
                    TknTy::Ident(ref name) => {
                        let sym = self.symtab.retrieve_from_finalized_sc(name, final_sc).unwrap();
                        return sym.ty_rec.ty.clone().unwrap();
                    },
                    _ => {
                        return prim_ty_rec.ty.clone().unwrap();
                    }
                }
            },
            Ast::ClassDecl{ident_tkn, methods:_, props:_, sc:_} => {
                let sym = self.symtab.retrieve(&ident_tkn.get_name()).unwrap();
                sym.ty_rec.ty.clone().unwrap()
            },
            Ast::ClassGet{class_tkn, prop_tkn} => {
                let class_sym = self.symtab.retrieve(&class_tkn.get_name()).unwrap();
                let class_decl_ast = class_sym.assign_val.clone().unwrap();

                self.extract_prop_ty(&class_decl_ast, &prop_tkn)
            },
            _ => {
                panic!("Unrecognized expression type found!")
            }
        }
    }

    /// Given a class declaration, retrieve the type of a property in the class. Because
    /// a class does not maintain a mapping of properties (right now), we loop through all
    /// available props until we find the name of the expected prop (the second param).
    fn extract_prop_ty(&self, class_decl_ast: &Ast, prop_name_tkn: &Token) -> TyName {
        let prop_name = prop_name_tkn.get_name();
        match class_decl_ast {
            Ast::ClassDecl{ident_tkn:_, methods:_, props, sc:_} => {
                let mut prop_ty = None;
                // TODO: this is O(n) and not efficient (should store props in a map)
                for prop in props {
                    match prop.clone().unwrap() {
                        Ast::VarDecl{ref ty_rec, ref ident_tkn, is_imm:_, is_global:_}
                        if ident_tkn.get_name() == prop_name => {
                            prop_ty = Some(ty_rec.ty.clone().unwrap());
                        },
                        Ast::VarAssign{ref ty_rec, ref ident_tkn, is_imm:_, is_global:_, value:_}
                        if ident_tkn.get_name() == prop_name  => {
                            prop_ty = Some(ty_rec.ty.clone().unwrap());
                        },
                        _ => ()
                    };
                }

                if prop_ty.is_none() {
                    panic!("Cannot type check variable that isnt declared in class");
                }

                return prop_ty.unwrap();
            },
            _ => panic!("Cannot extract property type from this ast")
        }
    }

    fn check_fn_params(&mut self, fn_call_ast: Ast, final_sc: usize) {
        match fn_call_ast {
            Ast::FnCall{fn_tkn, fn_params} => {
                let fn_sym = self.find_fn_sym(&fn_tkn.clone(), final_sc);
                let fn_param_tys = &fn_sym.unwrap().fn_params.clone().unwrap();

                let mut passed_in_param_tys = Vec::new();

                for ast in &fn_params {
                    passed_in_param_tys.push(self.check_expr(ast.clone(), 0));
                }

                for (idx, mb_ty_rec) in fn_param_tys.iter().enumerate() {
                    let ty_name = mb_ty_rec.clone().ty.unwrap();
                    if passed_in_param_tys[idx] != ty_name {
                        self.ty_mismatch(&fn_tkn.clone(),
                                         &passed_in_param_tys[idx],
                                         &ty_name);
                    }
                }
            },
            Ast::ClassFnCall{class_tkn:_, class_name:_, fn_tkn, fn_params, sc} => {
                let fn_sym = self.find_fn_sym(&fn_tkn.clone(), sc);
                let fn_param_tys = &fn_sym.unwrap().fn_params.clone().unwrap();

                let mut passed_in_param_tys = Vec::new();

                for ast in &fn_params {
                    passed_in_param_tys.push(self.check_expr(ast.clone(), 0));
                }

                for (idx, mb_ty_rec) in fn_param_tys.iter().enumerate() {
                    let ty_name = mb_ty_rec.clone().ty.unwrap();
                    if passed_in_param_tys[idx] != ty_name {
                        self.ty_mismatch(&fn_tkn.clone(),
                                         &passed_in_param_tys[idx],
                                         &ty_name);
                    }
                }
            },
            _ => ()
        }
    }

    /// Reduce a unary ast to the expected type to be returned by the expression.
    fn reduce_unary_ty(&mut self, op_tkn: Token, rhs_ty: TyName) -> TyName {
        match op_tkn.ty {
            TknTy::Minus => {
                if rhs_ty != TyName::Num {
                    self.ty_mismatch(&op_tkn, &TyName::Num, &rhs_ty);
                }
                TyName::Num
            },
            TknTy::Bang => {
                if rhs_ty != TyName::Bool {
                    self.ty_mismatch(&op_tkn, &TyName::Bool, &rhs_ty);
                }
                TyName::Bool
            },
            _ => panic!("Unimplemented unary operator found!")
        }
    }

    /// Reduce a binary ast so we can check the types in it. Returns the expected type
    /// given the operator, even if there is an error. The expected type is one which we expect
    /// the given operator to evaluate to.
    fn reduce_bin_ty(&mut self, op_tkn: Token, lhs_ty: TyName, rhs_ty: TyName) -> TyName {
        match op_tkn.ty {
            TknTy::Plus | TknTy::Minus | TknTy::Star | TknTy::Slash => {
                // We can only operate on types of the same kind
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    return TyName::Num;
                }

                // Ensure that the types are correct for the given operator
                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                }

                TyName::Num
            },
            TknTy::Gt | TknTy::GtEq | TknTy::Lt | TknTy::LtEq | TknTy::EqEq | TknTy::BangEq => {
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    return TyName::Bool;
                }

                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                }

                TyName::Bool
            },
            TknTy::And | TknTy::Or | TknTy::AmpAmp | TknTy::PipePipe => {
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    return TyName::Bool;
                }

                if !lhs_ty.is_bool() || !rhs_ty.is_bool() {
                    self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                }

                TyName::Bool
            },
            _ => panic!("Unimplemented binary operator found!")
        }
    }

    fn check_var_assign(&mut self, stmt: Ast, sc: usize) -> TyName {
        match stmt {
            Ast::VarAssign{ty_rec, ident_tkn, is_imm:_, is_global:_, value} => {
                let lhs_ty = ty_rec.ty.unwrap();
                let rhs = value.unwrap();

                let rhs_ty = self.check_expr(rhs, sc);
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&ident_tkn, &lhs_ty, &rhs_ty);
                }

                return lhs_ty;
            },
            _ => panic!("Invalid ast found when checking variable assignment")
        }
    }

    fn ty_mismatch(&mut self, tkn: &Token, lhs: &TyName, rhs: &TyName) {
        self.error(tkn.line, tkn.pos, TypeErrTy::TyMismatch(lhs.to_string(), rhs.to_string()));
    }

    fn op_mismatch(&mut self, tkn: &Token, lhs: &TyName, rhs: &TyName) {
        let ty = tkn.ty.clone();
        let op_desired = if ty.is_numerical_op() {
            (TyName::Num, TyName::Num)
        } else {
            (TyName::Bool, TyName::Bool)
        };

        let err_ty = TypeErrTy::BinOpMismatch(
            tkn.ty.to_string(),
            op_desired.0.to_string(),
            op_desired.1.to_string(),
            lhs.to_string(),
            rhs.to_string());

        self.error(tkn.line, tkn.pos, err_ty);
    }

    fn find_fn_sym(&mut self, ident_tkn: &Token, sc: usize) -> Option<Rc<Sym>> {
        let name = ident_tkn.get_name();
        let sym = self.symtab.retrieve_from_finalized_sc(&name, sc);
        match sym {
            Some(symbol) => Some(symbol),
            None => {
                self.error(ident_tkn.line, ident_tkn.pos, TypeErrTy::InvalidFn(name));
                None
            }
        }
    }

    fn error(&mut self, line: usize, pos: usize, ty: TypeErrTy) {
        let err = TypeErr::new(line, pos, ty);
        self.errors.push(err);
    }
}
