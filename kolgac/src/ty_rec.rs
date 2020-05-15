use crate::token::{TknTy, Token};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum KolgaTy {
    String,
    Num,
    Bool,
    Void,
    Symbolic(String),
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
            KolgaTy::Symbolic(name) => format!("symbolic '{}'", name),
        };

        write!(f, "{}", display_ty)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyRecord {
    pub name: String,
    pub ty: KolgaTy,
    pub tkn: Token,
}

impl TyRecord {
    pub fn new(tkn: Token, sym_count: usize) -> TyRecord {
        let name = format!("T{}", sym_count);
        let ty = match tkn.ty {
            TknTy::Num => KolgaTy::Num,
            TknTy::String => KolgaTy::String,
            TknTy::Str(_) => KolgaTy::String,
            TknTy::Val(_) => KolgaTy::Num,
            TknTy::Bool => KolgaTy::Bool,
            TknTy::True | TknTy::False => KolgaTy::Bool,
            TknTy::Minus => KolgaTy::Num,
            TknTy::Bang => KolgaTy::Bool,
            TknTy::Void => KolgaTy::Void,
            TknTy::Ident(ref ident) => KolgaTy::Class(ident.clone()),
            _ => KolgaTy::Symbolic(name),
        };

        TyRecord {
            name: format!("T{}", sym_count),
            ty: ty,
            tkn: tkn.clone(),
        }
    }

    pub fn unknown(tkn: Token, sym_count: usize) -> TyRecord {
        let name = format!("T{}", sym_count);

        TyRecord {
            name: name.clone(),
            ty: KolgaTy::Symbolic(name),
            tkn: tkn.clone(),
        }
    }
}
