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
    sym_ty: SymTy,
    val_ty: Option<ValTy>,
    name: String,
    val: Option<SymVal>
}

impl Sym {
    pub fn new(ty: SymTy, vty: Option<ValTy>, n: &str, v: Option<SymVal>) -> Sym {
        Sym {
            sym_ty: ty,
            val_ty: vty,
            name: n.to_string(),
            val: v
        }
    }
}
