pub enum TyVal {
    Str,
    Num,
    Bool
}

pub struct Reg {
    ty: Option<TyVal>,
    name: String,
}

impl Reg {
    pub fn new(n: &str) -> Reg {
        Reg {
            ty: None,
            name: n.to_string()
        }
    }
}
