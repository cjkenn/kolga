use token::{Token, TknTy};

#[derive(Clone, Debug, PartialEq)]
pub enum TyName {
    String,
    Num,
    Bool
}

impl TyName {
    pub fn is_numerical(&self) -> bool {
        match self {
            TyName::Num => true,
            _ => false
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            TyName::Bool => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyRecord {
    pub ty: Option<TyName>,
    pub tkn: Token
}

impl TyRecord {
    pub fn new_from_tkn(tkn: Token) -> TyRecord {
        let ty = match tkn.ty {
            TknTy::Num => Some(TyName::Num),
            TknTy::String => Some(TyName::String),
            TknTy::Str(_) => Some(TyName::String),
            TknTy::Val(_) => Some(TyName::Num),
            TknTy::Bool => Some(TyName::Bool),
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
