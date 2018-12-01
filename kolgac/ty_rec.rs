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
        };

        write!(f, "{}", display_ty)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeRecord {
    pub ty: Option<KolgaTy>,
    pub tkn: Token,
}

impl TypeRecord {
    pub fn new(tkn: Token) -> TypeRecord {
        let ty = match tkn.ty {
            TknTy::Num => Some(KolgaTy::Num),
            TknTy::String => Some(KolgaTy::String),
            TknTy::Str(_) => Some(KolgaTy::String),
            TknTy::Val(_) => Some(KolgaTy::Num),
            TknTy::Bool => Some(KolgaTy::Bool),
            TknTy::True | TknTy::False => Some(KolgaTy::Bool),
            TknTy::Minus => Some(KolgaTy::Num),
            TknTy::Bang => Some(KolgaTy::Bool),
            TknTy::Void => Some(KolgaTy::Void),
            TknTy::Ident(ref ident) => Some(KolgaTy::Class(ident.clone())),
            _ => None,
        };

        TypeRecord {
            ty: ty,
            tkn: tkn.clone(),
        }
    }

    pub fn empty(tkn: &Token) -> TypeRecord {
        TypeRecord {
            ty: None,
            tkn: tkn.clone(),
        }
    }
}
