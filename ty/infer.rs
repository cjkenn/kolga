use kolgac::ast::Ast;
use kolgac::symtab::SymbolTable;
use kolgac::ty_rec::{TyName, TyRec};
use error::ty::TypeErr;

pub struct TyInfer<'t, 's> {
    ast: &'t Ast,
    sym_tab: &'s SymbolTable,
    ty_count: usize,
    errors: Vec<TypeErr>
}

impl<'t, 's> TyInfer<'t, 's> {
    pub fn new(ast: &'t Ast, st: &'s mut SymbolTable) -> TyInfer<'t, 's> {
        TyInfer {
            ast: ast,
            sym_tab: st,
            ty_count: 0,
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
                    self.assign_stmt(stmt);
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

    fn assign_stmt(&mut self, stmt: &Ast) {
        match stmt {
            Ast::ExprStmt(ast) => {
                match *ast.clone() {
                    Ast::FnCall{fn_tkn:_, fn_params:_} => (),
                    Ast::ClassFnCall{class_tkn:_, class_name:_, fn_tkn:_, fn_params:_, sc:_} => (),
                    _ => {
                        self.assign_expr(&mut ast.clone());
                        ()
                    }
                }
            },
            _ => ()
        }
    }

    fn assign_expr(&mut self, expr: &mut Ast) {
        match expr {
            Ast::LogicalExpr{ref mut ty_rec, op_tkn:_, lhs:_, rhs:_} |
            Ast::BinaryExpr{ref mut ty_rec, op_tkn:_, lhs:_, rhs:_} => {
                self.set_ty(ty_rec);
            },
            _ => ()
        }
    }

    fn set_ty(&mut self, ty_rec: &mut TyRec) {
        let new_ty = self.curr_symbolic_ty();
        ty_rec.update(Some(TyName::Symbolic(new_ty)));

    }

    fn curr_symbolic_ty(&mut self) -> String {
        self.ty_count = self.ty_count + 1;
        format!("T{}", self.ty_count)
    }
}
