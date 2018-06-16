use token::Token;
use ast::Ast;
use type_record::TyRecord;

pub enum SymTy {
    Var,
    Func,
    Class,
}

pub struct Sym {
    pub sym_ty: SymTy,
    pub imm: bool,
    pub ty_rec: TyRecord,
    pub ident_tkn: Token,
    pub assign_val: Option<Ast>
}

impl Sym {
    pub fn new(sym_ty: SymTy,
               imm: bool,
               ty_rec: TyRecord,
               ident_tkn: Token,
               rhs: Option<Ast>) -> Sym {
        Sym {
            sym_ty: sym_ty,
            imm: imm,
            ty_rec: ty_rec,
            ident_tkn: ident_tkn,
            assign_val: rhs
        }
    }
}
