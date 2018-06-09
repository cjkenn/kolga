use snowc::ast::Ast;
use errors::ErrC;

pub struct TyCheck<'t> {
    ast: &'t Ast
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
            let tyerr = self.check_stmt(stmt);
            match tyerr {
                Some(err) => errs.push(err),
                _ => ()
            }
        }

        errs
    }

    fn check_stmt(&self, stmt: &Ast) -> Option<ErrC> {
        unimplemented!()
    }

    fn extract_head(&self) -> &Vec<Ast> {
        match self.ast {
            Ast::Prog(stmts) => stmts,
            _ => panic!("Cannot type check an ast with no statements")
        }
    }
}
