use crate::{ast::Ast, token::Token, ty_rec::TyRecord};

#[derive(Clone, Debug, PartialEq)]
pub enum SymTy {
    Param,
    Var,
    Fn,
    Class,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Sym {
    pub sym_ty: SymTy,
    pub imm: bool,
    pub ty_rec: TyRecord,
    pub ident_tkn: Token,
    pub assign_val: Option<Ast>,
    pub fn_params: Option<Vec<TyRecord>>,
}

impl Sym {
    pub fn new(
        sym_ty: SymTy,
        imm: bool,
        ty_rec: TyRecord,
        ident_tkn: Token,
        rhs: Option<Ast>,
        params: Option<Vec<TyRecord>>,
    ) -> Sym {
        Sym {
            sym_ty: sym_ty,
            imm: imm,
            ty_rec: ty_rec,
            ident_tkn: ident_tkn,
            assign_val: rhs,
            fn_params: params,
        }
    }
}
