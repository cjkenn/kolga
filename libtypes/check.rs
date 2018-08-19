use std::rc::Rc;

use kolgac::ast::Ast;
use kolgac::token::{Token, TknTy};
use kolgac::symtab::SymbolTable;
use kolgac::type_record::TyName;
use kolgac::sym::Sym;
use errors::ErrC;

pub struct TyCheck<'t, 's> {
    ast: &'t Ast,
    symtab: &'s mut SymbolTable,
    errors: Vec<ErrC>
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
    pub fn check(&mut self) -> Vec<ErrC> {
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
            Ast::VarDecl{ty_rec: _, ident_tkn: _, is_imm: _} => (),
            Ast::VarAssign{ty_rec: _, ident_tkn: _, is_imm:_, value: _} => {
                self.check_var_assign(stmt);
            },
            Ast::ExprStmt(ref maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                match ast {
                    Ast::FnCall(_, _) => {
                        self.check_fn_params(ast, final_sc);
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
            Ast::ForStmt(decl_ast, cond_expr, incr_expr, stmts) => {
                if decl_ast.is_some() {
                    self.check_stmt(decl_ast.clone().unwrap(), final_sc);
                }

                if cond_expr.is_some() {
                    self.check_stmt(cond_expr.clone().unwrap(), final_sc);
                }

                if incr_expr.is_some() {
                    self.check_stmt(incr_expr.clone().unwrap(), final_sc);
                }

                self.check_stmt(stmts.clone().unwrap(), final_sc);
            },
            Ast::BlckStmt{stmts, scope_lvl} => {
                for stmt in &stmts {
                    self.check_stmt(stmt.clone().unwrap(), scope_lvl);
                }
            },
            Ast::FuncDecl{ident_tkn, params: _, ret_ty, func_body, scope_lvl} => {
                let fn_ty = ret_ty.ty.unwrap();
                let fn_stmts = func_body.unwrap();
                match fn_stmts {
                    Ast::BlckStmt{stmts, scope_lvl: inner_sc} => {
                        self.check_fn_stmts(&ident_tkn, fn_ty, stmts, inner_sc);
                    },
                    _ => self.check_stmt(fn_stmts.clone(), scope_lvl)
                };
            },
            Ast::ClassDecl(_, methods, props) => {
                for prop_stmt in props {
                    self.check_stmt(prop_stmt.clone().unwrap(), final_sc);
                }

                for stmt in &methods {
                    self.check_stmt(stmt.clone().unwrap(), final_sc);
                }
            },
            _ => {
                panic!("Unrecognized statement type found!")
            }
        }
    }

    fn check_fn_stmts(&mut self, fn_tkn: &Token, fn_ret_ty: TyName, stmts: Vec<Option<Ast>>, sc_lvl: usize) {
        for maybe_stmt in &stmts {
            let stmt = maybe_stmt.clone().unwrap();
            match stmt {
                Ast::RetStmt(maybe_expr) => {
                    let rhs_ty = self.check_expr(maybe_expr.clone().unwrap(), sc_lvl);
                    if fn_ret_ty != rhs_ty {
                        let err = self.ty_mismatch(&fn_tkn, &fn_ret_ty, &rhs_ty);
                        self.errors.push(err);
                    }
                },
                _ => self.check_stmt(stmt, sc_lvl)
            };
        }
    }

    fn check_expr(&mut self, expr: Ast, final_sc: usize) -> TyName {
        match expr {
            Ast::VarAssign{ty_rec: _, ident_tkn: _, is_imm: _, value: _} => {
                self.check_var_assign(expr)
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
            Ast::ClassDecl(name_tkn,_,_) => {
                let sym = self.symtab.retrieve(&name_tkn.get_name()).unwrap();
                sym.ty_rec.ty.clone().unwrap()
            },
            Ast::ClassGet(class_name_tkn, prop_tkn) => {
                let class_sym = self.symtab.retrieve(&class_name_tkn.unwrap().get_name()).unwrap();
                let class_decl_ast = class_sym.assign_val.clone().unwrap();

                self.extract_prop_ty(&class_decl_ast, &prop_tkn.unwrap())
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
            Ast::ClassDecl(_, _, props) => {
                let mut prop_ty = None;
                // TODO: this is O(n) and not efficient
                for prop in props {
                    match prop.clone().unwrap() {
                        Ast::VarDecl{ref ty_rec, ref ident_tkn, is_imm: _}
                        if ident_tkn.get_name() == prop_name => {
                            prop_ty = Some(ty_rec.ty.clone().unwrap());
                        },
                        Ast::VarAssign{ref ty_rec, ref ident_tkn, is_imm: _, value: _}
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
            Ast::FnCall(name_tkn, params) => {
                let fn_sym = self.find_fn_sym(&name_tkn.clone().unwrap(), final_sc);
                let fn_param_tys = &fn_sym.unwrap().fn_params.clone().unwrap();

                let mut passed_in_param_tys = Vec::new();

                for ast in &params {
                    passed_in_param_tys.push(self.check_expr(ast.clone(), 0));
                }

                for (idx, mb_ty_rec) in fn_param_tys.iter().enumerate() {
                    let ty_name = mb_ty_rec.clone().ty.unwrap();
                    if passed_in_param_tys[idx] != ty_name {
                        let err = self.ty_mismatch(&name_tkn.clone().unwrap(),
                                                   &passed_in_param_tys[idx],
                                                   &ty_name);
                        self.errors.push(err);
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
                    let err = self.ty_mismatch(&op_tkn, &TyName::Num, &rhs_ty);
                    self.errors.push(err);
                }
                return TyName::Num;
            },
            TknTy::Bang => {
                if rhs_ty != TyName::Bool {
                    let err = self.ty_mismatch(&op_tkn, &TyName::Bool, &rhs_ty);
                    self.errors.push(err);
                }
                return TyName::Bool;
            },
            _ => panic!("Unimplemented unary operator found!")
        };
    }

    /// Reduce a binary ast so we can check the types in it. Returns the expected type
    /// given the operator, even if there is an error. The expected type is one which we expect
    /// the given operator to evaluate to.
    fn reduce_bin_ty(&mut self, op_tkn: Token, lhs_ty: TyName, rhs_ty: TyName) -> TyName {
        match op_tkn.ty {
            TknTy::Plus | TknTy::Minus | TknTy::Star | TknTy::Slash => {
                // We can only operate on types of the same kind
                if lhs_ty != rhs_ty {
                    let err = self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                // Ensure that the types are correct for the given operator
                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    let err = self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return TyName::Num
            },
            TknTy::Gt | TknTy::GtEq | TknTy::Lt | TknTy::LtEq | TknTy::EqEq | TknTy::BangEq => {
                if lhs_ty != rhs_ty {
                    let err = self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    let err = self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return TyName::Bool
            },
            TknTy::And | TknTy::Or | TknTy::AmpAmp | TknTy::PipePipe => {
                if lhs_ty != rhs_ty {
                    let err = self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                if !lhs_ty.is_bool() || !rhs_ty.is_bool() {
                    let err = self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return TyName::Bool
            },
            _ => panic!("Unimplemented binary operator found!")
        }
    }

    fn check_var_assign(&mut self, stmt: Ast) -> TyName {
        match stmt {
            Ast::VarAssign{ty_rec, ident_tkn, is_imm: _, value} => {
                let lhs_ty = ty_rec.ty.unwrap();
                let rhs = value.unwrap();

                let rhs_ty = self.check_expr(rhs, 0);
                if lhs_ty != rhs_ty {
                    let err = self.ty_mismatch(&ident_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return lhs_ty;
            },
            _ => panic!("Invalid ast found when checking variable assignment")
        }
    }

    fn ty_mismatch(&self, tkn: &Token, lhs: &TyName, rhs: &TyName) -> ErrC {
        let msg = format!("Type mismatch: Wanted {:?}, but found {:?}", lhs, rhs);
        ErrC::new(tkn.line, tkn.pos, msg)
    }

    fn op_mismatch(&self, tkn: &Token, lhs: &TyName, rhs: &TyName) -> ErrC {
        let ty = tkn.ty.clone();
        let op_desired = if ty.is_numerical_op() {
            (TyName::Num, TyName::Num)
        } else {
            (TyName::Bool, TyName::Bool)
        };

        let msg = format!("Operator mismatch: {:?} wants {:?} and {:?}, but found {:?} and {:?}",
                          tkn.ty,
                          op_desired.0,
                          op_desired.1,
                          lhs,
                          rhs);
        ErrC::new(tkn.line, tkn.pos, msg)
    }

    fn find_fn_sym(&mut self, ident_tkn: &Token, scope_lvl: usize) -> Option<Rc<Sym>> {
        let name = ident_tkn.get_name();
        let sym = self.symtab.retrieve_from_finalized_sc(&name, scope_lvl);
        match sym {
            Some(symbol) => Some(symbol),
            None => {
                let err_msg = format!("types: Undeclared function symbol {:?} found", name);
                self.errors.push(ErrC::new(ident_tkn.line, ident_tkn.pos, err_msg));
                None
            }
        }
    }
}
