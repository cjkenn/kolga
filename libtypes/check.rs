use snowc::ast::Ast;
use snowc::token::{Token, TknTy};
use errors::ErrC;

pub struct TyCheck<'t> {
    ast: &'t Ast
}

struct TyExt {
    pub lty: TknTy,
    pub rty: Option<TknTy>
}

impl TyExt {
    pub fn new(l: TknTy, r: Option<TknTy>) -> TyExt {
        TyExt {
            lty: l,
            rty: r
        }
    }

    pub fn is_unr_ty(&self) -> bool {
        self.rty.is_none()
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

        if assign_tyext.is_unr_ty() {
            if !self.match_tkn_ty(&exp_ty, &assign_tyext.lty) {
                return Some(self.ty_err(&tkn, exp_ty, assign_tyext.lty));
            }
        } else {
            // TODO: This should be a call to check_expr
            let assign_rty = assign_tyext.rty.unwrap();
            // Expression valid types
            if !self.match_tkn_ty(&assign_tyext.lty, &assign_rty) {
                return Some(self.ty_err(&tkn, assign_tyext.lty, assign_rty));
            }

            // If the expression is valid, check that the expr evaluated
            // type matches the var
            if !self.match_tkn_ty(&exp_ty, &assign_tyext.lty) {
                return Some(self.ty_err(&tkn, exp_ty, assign_tyext.lty));
            }
        }

        None
    }

    fn check_unary(&self, stmt: &Ast) -> Option<ErrC> {
        let op_tkn = self.extract_varop_tkn(stmt);
        let op_ty = op_tkn.ty.clone();

        let unr_tyext = self.extract_expr_ty(&stmt);

        if unr_tyext.is_unr_ty() {
            if !self.match_unr_op_ty(&op_ty, &unr_tyext.lty) {
                return Some(self.ty_err(&op_tkn, op_ty.to_ty(), unr_tyext.lty))
            }
        } else {
            let unr_rty = unr_tyext.rty.unwrap();
            if !self.match_unr_op_ty(&unr_tyext.lty, &unr_rty) {
                return Some(self.ty_err(&op_tkn, unr_tyext.lty, unr_rty));
            }

            if !self.match_unr_op_ty(&op_ty, &unr_tyext.lty) {
                return Some(self.ty_err(&op_tkn, op_ty.to_ty(), unr_tyext.lty))
            }
        }

        None
    }

    fn extract_expr_ty(&self, stmt: &Ast) -> TyExt {
        match stmt {
            Ast::Primary(ref tkn) => {
                TyExt::new(tkn.ty.clone(), None)
            },
            Ast::Unary(_, ref rhs) => {
                let lhsty = self.extract_expr_ty(&rhs.clone().unwrap()).lty;
                TyExt::new(lhsty, None)
            },
            Ast::Binary(_, ref lhs, ref rhs) => {
                let lhsty = self.extract_expr_ty(&lhs.clone().unwrap()).lty;
                let rhsty = self.extract_expr_ty(&rhs.clone().unwrap()).lty;
                TyExt::new(lhsty, Some(rhsty))
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

    fn match_unr_op_ty(&self, lty: &TknTy, rty: &TknTy) -> bool {
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
