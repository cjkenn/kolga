use snowc::ast::Ast;
use snowc::token::{Token, TknTy};
use errors::ErrC;

pub struct TyCheck<'t> {
    ast: &'t Ast,
    errors: Vec<ErrC>
}

impl<'t> TyCheck<'t> {
    pub fn new(a: &'t Ast) -> TyCheck {
        TyCheck {
            ast: a,
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
            Ast::VarDecl(_, _, _) => (), // ignore var assign
            Ast::VarAssign(_, _, _, _) => {
                self.check_var_assign(stmt);
            },
            Ast::ExprStmt(ref ast) => {
                self.check_expr(ast.clone().unwrap());
            },
            _ => panic!("Unrecognized statement type found!")
        }
    }

    fn check_expr(&mut self, expr: Ast) -> TknTy {
        match expr {
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

                    return self.eval_bin_ty(op_tkn.clone(), lhs_tkn.ty, rhs_tkn.ty);
                } else if lhs.is_primary() && !rhs.is_primary() {
                    let lhs_tkn = lhs.extract_primary_tkn();
                    let rhs_ty = self.check_expr(rhs);

                    return self.eval_bin_ty(op_tkn.clone(), lhs_tkn.ty, rhs_ty);
                } else if !lhs.is_primary() && rhs.is_primary() {
                    let lhs_ty = self.check_expr(lhs);
                    let rhs_tkn = rhs.extract_primary_tkn();

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_tkn.ty);
                } else {
                    let lhs_ty = self.check_expr(lhs);
                    let rhs_ty = self.check_expr(rhs);

                    return self.eval_bin_ty(op_tkn.clone(), lhs_ty, rhs_ty);
                }
            },
            Ast::Primary(prim_tkn) => {
                return prim_tkn.ty;
            }
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
}
