use token::Token;

pub enum Ast {
    Prog(Vec<Ast>),
    Assign(Token, bool, Box<Ast>)
}
