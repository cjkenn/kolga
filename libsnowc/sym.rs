pub enum SymTy {
    Var,
    Func,
    Class,
}

pub enum ValTy {
    Str,
    Num,
    Bool
}

pub enum SymVal {
    Str(String),
    Num(f64),
    Bool(bool)
}

pub struct Sym {
    pub sym_ty: SymTy,
    pub val_ty: Option<ValTy>,
    pub name: String,
    pub val: Option<SymVal>,
    pub imm: bool
}

impl Sym {
    pub fn new(ty: SymTy, vty: Option<ValTy>, n: &str, v: Option<SymVal>, im: bool) -> Sym {
        Sym {
            sym_ty: ty,
            val_ty: vty,
            name: n.to_string(),
            val: v,
            imm: im
        }
    }
}
