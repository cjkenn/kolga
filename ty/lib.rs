extern crate kolgac;
extern crate error;

pub mod check;
pub mod infer;

use check::TyCheck;
use infer::TyInfer;
use kolgac::ast::Ast;
use kolgac::symtab::SymbolTable;
use error::ty::TypeErr;

pub struct TyManager<'t, 's> {
    ast: &'t Ast,
    sym_tab: &'s mut SymbolTable
}

impl<'t, 's> TyManager<'t, 's> {
    pub fn new(ast: &'t Ast, st: &'s mut SymbolTable) -> TyManager<'t, 's> {
        TyManager {
            ast: ast,
            sym_tab: st
        }
    }

    pub fn check(&mut self) -> Vec<TypeErr> {
        let mut checker = TyCheck::new(&self.ast, &mut self.sym_tab);
        checker.check()
    }

    pub fn infer(&mut self) -> Vec<TypeErr> {
        let mut inferrer = TyInfer::new(&self.ast, &mut self.sym_tab);
        inferrer.infer()
    }
}
