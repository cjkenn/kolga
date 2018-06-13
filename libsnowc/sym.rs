use token::Token;

pub enum SymTy {
    Var,
    Func,
    Class,
}
// TODO: probably need to store the associated rhs ast for these values
// at some point
pub struct Sym {
    pub sym_ty: SymTy,
    pub name: String,
    pub imm: bool,
    pub ty_tkn: Token,
    pub val_tkn: Token
}

impl Sym {
    pub fn new(sym_ty: SymTy, n: &str, im: bool, ty_tkn: Token, val_tkn: Token) -> Sym {
        Sym {
            sym_ty: sym_ty,
            name: n.to_string(),
            imm: im,
            ty_tkn: ty_tkn,
            val_tkn: val_tkn
        }
    }
}
