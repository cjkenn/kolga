use token::Token;

pub enum Ast {
    Prog(Vec<Ast>),
    // Type token, Ident token, mutability, value
    Assign(Option<Token>, Option<Token>, bool, Box<Option<Ast>>),

    // Operator token, expression being operated on
    Unary(Token, Box<Option<Ast>>),

    // Parenthesized expr
    ParenExpr(Box<Option<Ast>>),

    // Func name, params
    FnCall(Option<Token>, Vec<Ast>),

    // Name of the class, function/property name
    ClassCall(Option<Token>, Option<Token>),

    // Identifier/Literal token
    Primary(Token)
}
