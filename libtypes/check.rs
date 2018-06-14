use snowc::ast::Ast;
use snowc::token::{Token, TknTy};
use snowc::symtab::SymTab;
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
            Ast::VarDecl(_, _, _) => (), // ignore var declaration without assign
            Ast::VarAssign(_, _, _, _) => {
                self.check_var_assign(stmt);
            },
            Ast::ExprStmt(ref ast) => {
                self.check_expr(ast.clone().unwrap());
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
            _ => panic!("Unrecognized statement type found!")
        }
    }

    fn check_expr(&mut self, expr: Ast) -> TknTy {
        match expr {
            Ast::VarAssign(_, _, _, _) => {
                self.check_var_assign(expr)
            },
            Ast::Unary(op_tkn, maybe_rhs) => {
                let rhs = maybe_rhs.unwrap();

                if rhs.is_primary() {
                    let rhs_tkn = rhs.extract_primary_tkn();
                    return self.eval_unary_ty(op_tkn.clone(), rhs_tkn.ty);
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
                    let lhs_tkn = lhs.extract_primary_tkn();
                    let rhs_tkn = rhs.extract_primary_tkn();

                    let mut lhs_ty = lhs_tkn.ty.clone();
                    let mut rhs_ty = rhs_tkn.ty.clone();

                    if lhs_tkn.is_ident() {
                        match self.find_sym_ty(&lhs_tkn) {
                            Some(ty) => lhs_ty = ty,
                            None => ()
                        }
                    }

                    if rhs_tkn.is_ident() {
                        match self.find_sym_ty(&rhs_tkn) {
                            Some(ty) => rhs_ty = ty,
                            None => ()
                        }
                    }

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                } else if lhs.is_primary() && !rhs.is_primary() {
                    let lhs_tkn = lhs.extract_primary_tkn();

                    let mut lhs_ty = lhs_tkn.ty.clone();
                    if lhs_tkn.is_ident() {
                        match self.find_sym_ty(&lhs_tkn) {
                            Some(ty) => lhs_ty = ty,
                            None => ()
                        }
                    }

                    let rhs_ty = self.check_expr(rhs);

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                } else if !lhs.is_primary() && rhs.is_primary() {
                    let lhs_ty = self.check_expr(lhs);
                    let rhs_tkn = rhs.extract_primary_tkn();

                    let mut rhs_ty = rhs_tkn.ty.clone();
                    if rhs_tkn.is_ident() {
                        match self.find_sym_ty(&rhs_tkn) {
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
            Ast::Primary(prim_tkn) => {
                match prim_tkn.ty {
                    TknTy::Ident(ref name) => {
                        let sym = self.symtab.retrieve(name).unwrap();
                        return sym.ty_tkn.ty.to_equiv_ty();
                    },
                    _ => {
                        return prim_tkn.ty.to_equiv_ty();
                    }
                }
            },
            _ => panic!()
        }
    }

    fn eval_unary_ty(&mut self, op_tkn: Token, rhs_ty: TknTy) -> TknTy {
        match op_tkn.ty {
            TknTy::Minus => {
                if rhs_ty.to_equiv_ty() != TknTy::Num {
                    let err = self.ty_mismatch(&op_tkn, &TknTy::Num, &rhs_ty);
                    self.errors.push(err);
                }
                return TknTy::Num;
            },
            TknTy::Bang => {
                if rhs_ty.to_equiv_ty() != TknTy::Bool {
                    let err = self.ty_mismatch(&op_tkn, &TknTy::Bool, &rhs_ty);
                    self.errors.push(err);
                }
                return TknTy::Bool;
            },
            _ => panic!("Unimplemented unary operator found!")
        };
    }

    // Returns the expected type given the operator, even if there is an error.
    // The expected type is one which we expect the given operator to evaluate to.
    fn eval_bin_ty(&mut self, op_tkn: Token, lhs_ty: TknTy, rhs_ty: TknTy) -> TknTy {
        match op_tkn.ty {
            TknTy::Plus | TknTy::Minus | TknTy::Star | TknTy::Slash => {
                // We can only operate on types of the same kind
                if lhs_ty.to_equiv_ty() != rhs_ty.to_equiv_ty() {
                    let err = self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                // Ensure that the types are correct for the given operator
                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    let err = self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return TknTy::Num
            },
            TknTy::Gt | TknTy::GtEq | TknTy::Lt | TknTy::LtEq => {
                if lhs_ty.to_equiv_ty() != rhs_ty.to_equiv_ty() {
                    let err = self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    let err = self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return TknTy::Bool
            },
            TknTy::And | TknTy::Or | TknTy::AmpAmp | TknTy::PipePipe => {
                if lhs_ty.to_equiv_ty() != rhs_ty.to_equiv_ty() {
                    let err = self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                if !lhs_ty.is_bool() || !rhs_ty.is_bool() {
                    let err = self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return TknTy::Bool
            },
            _ => panic!("Unimplemented binary operator found!")
        }
    }

    fn check_var_assign(&mut self, stmt: Ast) -> TknTy {
        match stmt {
            Ast::VarAssign(var_ty_tkn, ident_tkn, _imm, maybe_rhs) => {
                let lhs_ty = var_ty_tkn.ty;
                let rhs = maybe_rhs.unwrap();

                let rhs_ty = self.check_expr(rhs);
                if lhs_ty.to_equiv_ty() != rhs_ty.to_equiv_ty() {
                    let err = self.ty_mismatch(&ident_tkn, &lhs_ty, &rhs_ty);
                    self.errors.push(err);
                }

                return lhs_ty;
            },
            _ => panic!("Invalid ast found when checking variable assignment")
        }
    }

    fn ty_mismatch(&self, tkn: &Token, lhs: &TknTy, rhs: &TknTy) -> ErrC {
        let msg = format!("Type mismatch: Wanted {:?}, but found {:?}",
                          lhs.to_equiv_ty(),
                          rhs.to_equiv_ty());
        ErrC::new(tkn.line, tkn.pos, msg)
    }

    fn op_mismatch(&self, tkn: &Token, lhs: &TknTy, rhs: &TknTy) -> ErrC {
        let ty = tkn.ty.clone();
        let op_desired = if ty.is_numerical_op() {
            (TknTy::Num, TknTy::Num)
        } else {
            (TknTy::Bool, TknTy::Bool)
        };

        let msg = format!("Operator mismatch: {:?} wants {:?} and {:?}, but found {:?} and {:?}",
                          tkn.ty,
                          op_desired.0,
                          op_desired.1,
                          lhs.to_equiv_ty(),
                          rhs.to_equiv_ty());
        ErrC::new(tkn.line, tkn.pos, msg)
    }

    fn find_sym_ty(&mut self, ident_tkn: &Token) -> Option<TknTy> {
        let name = ident_tkn.get_name();
        let sym = self.symtab.retrieve(&name);
        match sym {
            Some(symbol) => Some(symbol.ty_tkn.ty.to_equiv_ty()),
            None => {
                let err_msg = format!("Undeclared symbol {:?} found", name);
                self.errors.push(ErrC::new(ident_tkn.line, ident_tkn.pos, err_msg));
                None
            }
        }
    }
}
