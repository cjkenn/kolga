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
            // &Ast::VarAssign(_, _, _, _) => self.check_var_assign(stmt),
            Ast::ExprStmt(ref ast) => {
                self.check_expr(ast.clone().unwrap());
            },
            _ => ()
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
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) => {
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

    fn eval_unary_ty(&self, op_tkn: Token, rhs_ty: TknTy) -> TknTy {
        unimplemented!()
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
                return TknTy::Bool
            },
            _ => panic!()
        }
    }

    // fn check_var_assign(&self, stmt: &Ast) {
    //     let tkn = self.extract_varop_tkn(stmt);
    //     let exp_ty = tkn.ty.clone();

    //     // The value of an assignment can be an expression. We don't
    //     // need to evaluate the expression, but we can get the types of
    //     // the expression operators and check them here.
    //     let assign_ast = match stmt {
    //         &Ast::VarAssign(_, _, _, ref ast) => {
    //             ast.clone().unwrap()
    //         },
    //         _ => panic!()
    //     };

    //     let assign_ty_pair = self.extract_expr_ty(&assign_ast);

    //     if !assign_ty_pair.is_single_ty() {
    //         // TODO: This should be a call to check_expr
    //         let assign_rhs_ty = assign_ty_pair.rhs_ty.unwrap();
    //         // Expression valid types
    //         if !self.match_tkn_ty(&assign_ty_pair.lhs_ty, &assign_rhs_ty) {
    //             return Some(self.ty_err(&tkn, assign_ty_pair.lhs_ty, assign_rhs_ty));
    //         }
    //     }

    //     // If the expression is valid, check that the expr evaluated
    //     // type matches the var
    //     if !self.match_tkn_ty(&exp_ty, &assign_ty_pair.lhs_ty) {
    //         return Some(self.ty_err(&tkn, exp_ty, assign_ty_pair.lhs_ty));
    //     }

    //     None
    // }

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
