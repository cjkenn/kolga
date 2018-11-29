pub mod gen;
pub mod lex;
pub mod parse;
pub mod ty;

pub trait KolgaErr {
    fn emit(&self);
    fn to_msg(&self) -> String;
}
