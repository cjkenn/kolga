use token::Token;


pub enum TyObj {
    // Var type, var name, is immutable, assignment value
    Var(Option<Token>, Option<Token>, bool, Expr),

    // Function name, params, return type, body
    Func(Option<Token>, Vec<Token>, Option<Token>, Vec<TyObj>),
    Add(Expr, Expr),
    Sub(Token, Token),
    Mul(Token, Token),
    Div(Token, Token)
}

pub enum Expr {
    Bin(Box<Expr>, Token, Box<Expr>),
    Unr(Token, Box<Expr>)
}
