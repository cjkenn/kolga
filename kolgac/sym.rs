use ast::Ast;
use token::Token;
use ty_rec::TypeRecord;

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
    pub ty_rec: TypeRecord,
    pub ident_tkn: Token,
    pub assign_val: Option<Ast>,
    pub fn_params: Option<Vec<TypeRecord>>,
}

impl Sym {
    pub fn new(
        sym_ty: SymTy,
        imm: bool,
        ty_rec: TypeRecord,
        ident_tkn: Token,
        rhs: Option<Ast>,
        params: Option<Vec<TypeRecord>>,
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
