use token::{Token, TknTy};

#[derive(Clone, Debug, PartialEq)]
pub enum TyName {
    String,
    Num,
    Bool
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyRecord {
    pub ty: Option<TyName>,
    pub tkn: Token
}

impl TyRecord {
    pub fn new_from_tkn(tkn: Token) -> TyRecord {
        let ty = match tkn.ty {
            TknTy::Str(_) => Some(TyName::String),
            TknTy::Val(_) => Some(TyName::Num),
            TknTy::True | TknTy::False => Some(TyName::Bool),
            TknTy::Minus => Some(TyName::Num),
            TknTy::Bang => Some(TyName::Bool),
            _ => None
        };

        TyRecord {
            ty: ty,
            tkn: tkn.clone(),
        }
    }
}
