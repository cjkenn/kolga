use std::fmt;
use token::{TknTy, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum KolgaTy {
    String,
    Num,
    Bool,
    Void,
    Class(String),
}

impl KolgaTy {
    pub fn new(tkn_ty: &TknTy) -> Option<KolgaTy> {
        // TODO: make a class ty here?
        match *tkn_ty {
            TknTy::Num => Option<KolgaTy>::Num,
            TknTy::Val(_) => Option<KolgaTy>::Num,
            TknTy::String => Option<KolgaTy>::String,
            TknTy::Str(_) => Option<KolgaTy>::String,
            TknTy::Bool => Option<KolgaTy>::Bool,
            TknTy::True | TknTy::False => Option<KolgaTy>::Bool,
            TknTy::Minus => Option<KolgaTy>::Num,
            TknTy::Bang => Option<KolgaTy>::Bool,
            TknTy::Void => Option<KolgaTy>::Void,
            _ => None,
        }
    }

    pub fn is_numerical(&self) -> bool {
        match self {
            KolgaTy::Num => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            KolgaTy::Bool => true,
            _ => false,
        }
    }
}

impl fmt::Display for KolgaTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let display_ty = match self {
            KolgaTy::String => "string".to_string(),
            KolgaTy::Num => "number".to_string(),
            KolgaTy::Bool => "bool".to_string(),
            KolgaTy::Void => "void".to_string(),
            KolgaTy::Class(name) => format!("class '{}'", name),
            KolgaTy::Symbolic(name) => format!("symbolic '{}'", name),
        };

        write!(f, "{}", display_ty)
    }
}
