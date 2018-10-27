use std::fmt;
use token::{Token, TknTy};

#[derive(Clone, Debug, PartialEq)]
pub enum TyName {
    String,
    Num,
    Bool,
    Void,
    Class(String)
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

impl fmt::Display for TyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let display_ty = match self {
            TyName::String => "string".to_string(),
            TyName::Num => "number".to_string(),
            TyName::Bool => "bool".to_string(),
            TyName::Void => "void".to_string(),
            TyName::Class(name) => format!("class '{}'", name)
        };

        write!(f, "{}", display_ty)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyRec {
    pub ty: Option<TyName>,
    pub tkn: Token
}

impl TyRec {
    pub fn new_from_tkn(tkn: Token) -> TyRec {
        let ty = match tkn.ty {
            TknTy::Num => Some(TyName::Num),
            TknTy::String => Some(TyName::String),
            TknTy::Str(_) => Some(TyName::String),
            TknTy::Val(_) => Some(TyName::Num),
            TknTy::Bool => Some(TyName::Bool),
            TknTy::True | TknTy::False => Some(TyName::Bool),
            TknTy::Minus => Some(TyName::Num),
            TknTy::Bang => Some(TyName::Bool),
            TknTy::Void => Some(TyName::Void),
            TknTy::Ident(ref ident) => Some(TyName::Class(ident.clone())),
            _ => None
        };

        TyRec {
            ty: ty,
            tkn: tkn.clone(),
        }
    }
}
