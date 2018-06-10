use snowc::ast::Ast;
use snowc::token::{Token, TknTy};
use errors::ErrC;

pub struct TyCheck<'t> {
    ast: &'t Ast
}

struct TyPair {
    pub lhs_ty: TknTy,
    pub rhs_ty: Option<TknTy>
}

impl TyPair {
    pub fn new(lt: TknTy, rt: Option<TknTy>) -> TyPair {
        TyPair {
            lhs_ty: lt,
            rhs_ty: rt
        }
    }

    pub fn is_single_ty(&self) -> bool {
        self.rhs_ty.is_none()
    }
}

impl<'t> TyCheck<'t> {
    pub fn new(a: &'t Ast) -> TyCheck {
        TyCheck {
            ast: a
        }
    }

    pub fn check(&self) -> Vec<ErrC> {
        let stmts = self.extract_head();
        let mut errs = Vec::new();

        for stmt in stmts {
            let err = self.check_stmt(stmt);
            match err {
                Some(e) => errs.push(e),
                _ => ()
            }
        }

        errs
    }

    fn check_stmt(&self, stmt: &Ast) -> Option<ErrC>  {
        match stmt {
            &Ast::VarAssign(_, _, _, _) => self.check_var_assign(stmt),
            &Ast::ExprStmt(ref ast) => self.check_expr(&ast.clone().unwrap()),
            _ => None
        }
    }

    fn check_expr(&self, expr: &Ast) -> Option<ErrC> {
        match expr {
            &Ast::Unary(_, _) => self.check_unary(expr),
            _ => None
        }
    }

    fn check_var_assign(&self, stmt: &Ast) -> Option<ErrC> {
        let tkn = self.extract_varop_tkn(stmt);
        let exp_ty = tkn.ty.clone();

        // The value of an assignment can be an expression. We don't
        // need to evaluate the expression, but we can get the types of
        // the expression operators and check them here.
        let assign_ast = match stmt {
            &Ast::VarAssign(_, _, _, ref ast) => {
                ast.clone().unwrap()
            },
            _ => panic!()
        };

        let assign_tyext = self.extract_expr_ty(&assign_ast);

        if assign_tyext.is_single_ty() {
            if !self.match_tkn_ty(&exp_ty, &assign_tyext.lhs_ty) {
                return Some(self.ty_err(&tkn, exp_ty, assign_tyext.lhs_ty));
            }
        } else {
            // TODO: This should be a call to check_expr
            let assign_rhs_ty = assign_tyext.rhs_ty.unwrap();
            // Expression valid types
            if !self.match_tkn_ty(&assign_tyext.lhs_ty, &assign_rhs_ty) {
                return Some(self.ty_err(&tkn, assign_tyext.lhs_ty, assign_rhs_ty));
            }

            // If the expression is valid, check that the expr evaluated
            // type matches the var
            if !self.match_tkn_ty(&exp_ty, &assign_tyext.lhs_ty) {
                return Some(self.ty_err(&tkn, exp_ty, assign_tyext.lhs_ty));
            }
        }

        None
    }

    fn check_unary(&self, expr: &Ast) -> Option<ErrC> {
        let op_tkn = self.extract_varop_tkn(expr);
        let op_ty = op_tkn.ty.clone();

        let unary_ty_pair = self.extract_expr_ty(&expr);

        // If we have two types in the pair, we need to match lhs ty and rhs ty
        // in the pair, and also the operator with one of the types
        if unary_ty_pair.is_single_ty() {
            if !self.match_unary_ty_pair(&op_ty, &unary_ty_pair.lhs_ty) {
                return Some(self.ty_err(&op_tkn, op_ty.to_ty(), unary_ty_pair.lhs_ty))
            }
        } else {
            let unr_rhs_ty = unary_ty_pair.rhs_ty.unwrap();
            if !self.match_unary_ty_pair(&unary_ty_pair.lhs_ty, &unr_rhs_ty) {
                return Some(self.ty_err(&op_tkn, unary_ty_pair.lhs_ty, unr_rhs_ty));
            }

            if !self.match_unary_ty_pair(&op_ty, &unary_ty_pair.lhs_ty) {
                return Some(self.ty_err(&op_tkn, op_ty.to_ty(), unary_ty_pair.lhs_ty))
            }
        }

        None
    }

    fn extract_expr_ty(&self, stmt: &Ast) -> TyPair {
        match stmt {
            Ast::Primary(ref tkn) => {
                TyPair::new(tkn.ty.clone(), None)
            },
            Ast::Unary(_, ref rhs) => {
                let lhsty = self.extract_expr_ty(&rhs.clone().unwrap()).lhs_ty;
                TyPair::new(lhsty, None)
            },
            Ast::Binary(_, ref lhs, ref rhs) => {
                let lhsty = self.extract_expr_ty(&lhs.clone().unwrap()).lhs_ty;
                let rhsty = self.extract_expr_ty(&rhs.clone().unwrap()).lhs_ty;
                TyPair::new(lhsty, Some(rhsty))
            }
            _ => panic!()
        }

    }

    fn match_tkn_ty(&self, lty: &TknTy, rty: &TknTy) -> bool {
        match *rty {
            TknTy::Str(_) => {
                *lty == TknTy::String || *lty == TknTy::Null || lty == rty
            },
            TknTy::Val(_) => {
                *lty == TknTy::Num || *lty == TknTy::Null || lty == rty
            },
            TknTy::True | TknTy::False => {
                *lty == TknTy::Bool || *lty == TknTy::Null || lty == rty
            },
            TknTy::Null => true,
            _ => false
        }
    }

    fn match_unary_ty_pair(&self, lty: &TknTy, rty: &TknTy) -> bool {
        match *rty {
            TknTy::Val(_) | TknTy::Num => *lty == TknTy::Minus,
            TknTy::True | TknTy::False | TknTy::Bool => *lty == TknTy::Bang,
            _ => false
        }
    }

    fn extract_head(&self) -> &Vec<Ast> {
        match self.ast {
            Ast::Prog(stmts) => stmts,
            _ => panic!("Cannot type check an ast with no statements")
        }
    }

    fn extract_varop_tkn(&self, stmt: &Ast) -> Token {
        match stmt {
            &Ast::VarAssign(ref tkn, _, _, _) => tkn.clone(),
            &Ast::Unary(ref tkn, _) => tkn.clone(),
            _ => panic!()
        }
    }

    fn ty_err(&self, tkn: &Token, lhs: TknTy, rhs: TknTy) -> ErrC {
        let msg = format!("Type mismatch: Wanted {:?}, but found {:?}", lhs.to_ty(), rhs.to_ty());
        ErrC::new(tkn.line, tkn.pos, msg)
    }
}
