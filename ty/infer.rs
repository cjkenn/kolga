use kolgac::ast::Ast;
use kolgac::symtab::SymbolTable;
use error::ty::TypeErr;

pub struct TyInfer<'t, 's> {
    ast: &'t Ast,
    sym_tab: &'s SymbolTable,
    errors: Vec<TypeErr>
}

impl<'t, 's> TyInfer<'t, 's> {
    pub fn new(ast: &'t Ast, st: &'s mut SymbolTable) -> TyInfer<'t, 's> {
        TyInfer {
            ast: ast,
            sym_tab: st,
            errors: Vec::new()
        }
    }

    pub fn infer(&mut self) -> Vec<TypeErr> {
        self.assign();
        self.gen_eq();
        self.unify();

        self.errors.clone()
    }

    fn assign(&mut self) {
        match self.ast {
            Ast::Prog{stmts} => {
                for stmt in stmts {
                    self.infer_stmt(stmt);
                }
            },
            _ => ()
        }
    }

    fn gen_eq(&mut self) {
        unimplemented!()
    }

    fn unify(&mut self) {
        unimplemented!()
    }

    fn infer_stmt(&mut self, stmt: &Ast) {
        unimplemented!()
    }
}
