use token::Token;
use ast::Ast;

pub enum SymTy {
    Var,
    Func,
    Class,
}

pub struct Sym {
    pub sym_ty: SymTy,
    pub imm: bool,
    pub ty_tkn: Token,
    pub ident_tkn: Token,
    pub assign_val: Option<Ast>
}

impl Sym {
    pub fn new(sym_ty: SymTy,
               imm: bool,
               ty_tkn: Token,
               ident_tkn: Token,
               rhs: Option<Ast>) -> Sym {
        Sym {
            sym_ty: sym_ty,
            imm: imm,
            ty_tkn: ty_tkn,
            ident_tkn: ident_tkn,
            assign_val: rhs
        }
    }
}
