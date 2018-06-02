use token::Token;

pub enum TyObj {
    Var(Token, Token),
    Add(Token, Token),
    Sub(Token, Token),
    Mul(Token, Token),
    Div(Token, Token)
}
