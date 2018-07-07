use std::rc::Rc;

use snowc::ast::Ast;
use snowc::token::{Token, TknTy};
use snowc::symtab::SymTab;
use snowc::type_record::TyName;
use snowc::sym::Sym;
use errors::ErrC;

pub struct TyCheck<'t, 's> {
    ast: &'t Ast,
    symtab: &'s mut SymTab,
    errors: Vec<ErrC>
}

impl<'t, 's> TyCheck<'t, 's> {
    pub fn new(ast: &'t Ast, symtab: &'s mut SymTab) -> TyCheck<'t, 's> {
        TyCheck {
            ast: ast,
            symtab: symtab,
            errors: Vec::new()
        }
    }

    pub fn check(&mut self) -> Vec<ErrC> {
        match *self.ast {
            Ast::Prog(ref stmts) => {
                for stmt in stmts {
                    // TODO: shouldnt clone this, pass reference
                    self.check_stmt(stmt.clone());
                }
            },
            _ => ()
        }

        self.errors.clone()
    }

    fn check_stmt(&mut self, stmt: Ast) {
        match stmt {
            Ast::VarDecl(_, _, _) => (), // ignore var declaration without assign (nothing to check)
            Ast::VarAssign(_, _, _, _) => {
                self.check_var_assign(stmt);
            },
            Ast::ExprStmt(ref maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                match ast {
                    Ast::FnCall(_, _) => {
                        self.check_fn_params(ast);
                        ()
                    },
                    _ => {
                        self.check_expr(ast);
                        ()
                    }
                };
            },
            Ast::IfStmt(expr_ast, maybe_stmt_ast, elif_stmts, maybe_el_stmts) => {
                self.check_expr(expr_ast.clone().unwrap());
                self.check_stmt(maybe_stmt_ast.clone().unwrap());

                for stmt in &elif_stmts {
                    self.check_stmt(stmt.clone().unwrap());
                }

                if maybe_el_stmts.is_some() {
                    self.check_stmt(maybe_el_stmts.clone().unwrap());
                }
            },
            Ast::WhileStmt(maybe_expr_ast, maybe_stmts) => {
                self.check_expr(maybe_expr_ast.clone().unwrap());
                self.check_stmt(maybe_stmts.clone().unwrap());
            },
            Ast::ElifStmt(maybe_expr_ast, maybe_stmt_ast) => {
                self.check_expr(maybe_expr_ast.clone().unwrap());
                self.check_stmt(maybe_stmt_ast.clone().unwrap());
            },
            Ast::ForStmt(decl_ast, cond_expr, incr_expr, stmts) => {
                if decl_ast.is_some() {
                    self.check_stmt(decl_ast.clone().unwrap());
                }

                if cond_expr.is_some() {
                    self.check_stmt(cond_expr.clone().unwrap());
                }

                if incr_expr.is_some() {
                    self.check_stmt(incr_expr.clone().unwrap());
                }

                self.check_stmt(stmts.clone().unwrap());
            },
            Ast::BlckStmt(stmts) => {
                for stmt in &stmts {
                    self.check_stmt(stmt.clone().unwrap());
                }
            },
            Ast::FnDecl(ident_tkn, _, ret_ty_rec, maybe_stmts) => {
                let fn_ty = ret_ty_rec.ty.unwrap();
                let stmts = maybe_stmts.unwrap();
                match stmts {
                    Ast::BlckStmt(stmt_list) => self.check_fn_stmts(ident_tkn.clone(), fn_ty, stmt_list),
                    _ => self.check_stmt(stmts.clone())
                };
            },
            Ast::ClassDecl(_, methods, props) => {
                for prop_stmt in props {
                    self.check_stmt(prop_stmt.clone().unwrap());
                }

                for stmt in &methods {
                    self.check_stmt(stmt.clone().unwrap());
                }
            },
            _ => {
                panic!("Unrecognized statement type found!")
            }
        }
    }

    fn check_fn_stmts(&mut self, fn_tkn: Token, fn_ret_ty: TyName, stmts: Vec<Option<Ast>>) {
        for maybe_stmt in &stmts {
            let stmt = maybe_stmt.clone().unwrap();
            match stmt {
                Ast::RetStmt(maybe_expr) => {
                    let rhs_ty = self.check_expr(maybe_expr.clone().unwrap());
                    if fn_ret_ty != rhs_ty {
                        let err = self.ty_mismatch(&fn_tkn, &fn_ret_ty, &rhs_ty);
                        self.errors.push(err);
                    }
                },
                _ => self.check_stmt(stmt)
            };
        }
    }

    fn check_expr(&mut self, expr: Ast) -> TyName {
        match expr {
            Ast::VarAssign(_, _, _, _) => {
                self.check_var_assign(expr)
            },
            Ast::Unary(op_tkn, maybe_rhs) => {
                let rhs = maybe_rhs.unwrap();

                if rhs.is_primary() {
                    let rhs_ty_rec = rhs.extract_primary_ty_rec();
                    return self.eval_unary_ty(op_tkn.clone(), rhs_ty_rec.ty.unwrap());
                } else {
                    let rhs_ty = self.check_expr(rhs);
                    return self.eval_unary_ty(op_tkn.clone(), rhs_ty);
                }
            },
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) |
            Ast::Logical(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs = maybe_lhs.unwrap();
                let rhs = maybe_rhs.unwrap();

                if lhs.is_primary() && rhs.is_primary() {
                    let lhs_ty_rec = lhs.extract_primary_ty_rec();
                    let rhs_ty_rec = rhs.extract_primary_ty_rec();

                    let mut lhs_ty = lhs_ty_rec.ty.unwrap().clone();
                    let mut rhs_ty = rhs_ty_rec.ty.unwrap().clone();

                    if lhs_ty_rec.tkn.is_ident() {
                        match self.find_sym_ty(&lhs_ty_rec.tkn) {
                            Some(ty) => lhs_ty = ty,
                            None => ()
                        }
                    }

                    if rhs_ty_rec.tkn.is_ident() {
                        match self.find_sym_ty(&rhs_ty_rec.tkn) {
                            Some(ty) => rhs_ty = ty,
                            None => ()
                        }
                    }

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                } else if lhs.is_primary() && !rhs.is_primary() {
                    let lhs_ty_rec = lhs.extract_primary_ty_rec();

                    let mut lhs_ty = lhs_ty_rec.ty.unwrap().clone();
                    if lhs_ty_rec.tkn.is_ident() {
                        match self.find_sym_ty(&lhs_ty_rec.tkn) {
                            Some(ty) => lhs_ty = ty,
                            None => ()
                        }
                    }

                    let rhs_ty = self.check_expr(rhs);

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                } else if !lhs.is_primary() && rhs.is_primary() {
                    let lhs_ty = self.check_expr(lhs);
                    let rhs_ty_rec = rhs.extract_primary_ty_rec();

                    let mut rhs_ty = rhs_ty_rec.ty.unwrap().clone();
                    if rhs_ty_rec.tkn.is_ident() {
                        match self.find_sym_ty(&rhs_ty_rec.tkn) {
                            Some(ty) => rhs_ty = ty,
                            None => ()
                        }
                    }

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                } else {
                    let lhs_ty = self.check_expr(lhs);
                    let rhs_ty = self.check_expr(rhs);

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                }
            },
            Ast::Primary(prim_ty_rec) => {
                match prim_ty_rec.tkn.ty {
                    TknTy::Ident(ref name) => {
                        let sym = self.symtab.retrieve(name).unwrap();
                        return sym.ty_rec.ty.clone().unwrap();
                    },
                    _ => {
                        return prim_ty_rec.ty.clone().unwrap();
                    }
                }
            },
            Ast::ClassDecl(name_tkn,_,_) => {
                let sym = self.symtab.retrieve(&name_tkn.get_name()).unwrap();
                return sym.ty_rec.ty.clone().unwrap();
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

    fn extract_prop_ty(&self, class_decl_ast: &Ast, prop_name_tkn: &Token) -> TyName {
        let prop_name = prop_name_tkn.get_name();

        match class_decl_ast {
            Ast::ClassDecl(_, _, props) => {
                let mut prop_ty = None;
                // TODO: this is O(n) and not efficient
                for prop in props {
                    match prop.clone().unwrap() {
                        Ast::VarDecl(ref ty_rec, ref tkn, _) if tkn.get_name() == prop_name => {
                            prop_ty = Some(ty_rec.ty.clone().unwrap());
                        },
                        Ast::VarAssign(ref ty_rec, ref tkn, _, _) if tkn.get_name() == prop_name  => {
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

    fn check_fn_params(&mut self, fn_call_ast: Ast) {
        match fn_call_ast {
            Ast::FnCall(name_tkn, params) => {
                let fn_sym = self.find_fn_sym(&name_tkn.clone().unwrap());
                let fn_param_tys = &fn_sym.unwrap().fn_params.clone().unwrap();

                let mut passed_in_param_tys = Vec::new();

                for ast in &params {
                    passed_in_param_tys.push(self.check_expr(ast.clone()));
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

    fn eval_unary_ty(&mut self, op_tkn: Token, rhs_ty: TyName) -> TyName {
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

    // Returns the expected type given the operator, even if there is an error.
    // The expected type is one which we expect the given operator to evaluate to.
    fn eval_bin_ty(&mut self, op_tkn: Token, lhs_ty: TyName, rhs_ty: TyName) -> TyName {
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
            TknTy::Gt | TknTy::GtEq | TknTy::Lt | TknTy::LtEq => {
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
            Ast::VarAssign(var_ty_rec, ident_tkn, _imm, maybe_rhs) => {
                let lhs_ty = var_ty_rec.ty.unwrap();
                let rhs = maybe_rhs.unwrap();

                let rhs_ty = self.check_expr(rhs);
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
        let msg = format!("Type mismatch: Wanted {:?}, but found {:?}",
                          lhs,
                          rhs);
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

    fn find_sym_ty(&mut self, ident_tkn: &Token) -> Option<TyName> {
        let name = ident_tkn.get_name();
        let sym = self.symtab.retrieve(&name);
        match sym {
            Some(symbol) => Some(symbol.ty_rec.ty.clone().unwrap()),
            None => {
                let err_msg = format!("Undeclared symbol {:?} found", name);
                self.errors.push(ErrC::new(ident_tkn.line, ident_tkn.pos, err_msg));
                None
            }
        }
    }

    fn find_fn_sym(&mut self, ident_tkn: &Token) -> Option<Rc<Sym>> {
        let name = ident_tkn.get_name();
        let sym = self.symtab.retrieve(&name);
        match sym {
            Some(symbol) => Some(symbol),
            None => {
                let err_msg = format!("Undeclared symbol {:?} found", name);
                self.errors.push(ErrC::new(ident_tkn.line, ident_tkn.pos, err_msg));
                None
            }
        }
    }
}
