use token::Token;

#[derive(Clone)]
pub enum Ast {
    Prog(Vec<Ast>),

    // List of statements/expression in a block
    BlckStmt(Vec<Option<Ast>>),

    IfStmt,

    WhileStmt,

    ForStmt,

    RetStmt,

    ExprStmt,

    // Var type tkn, var name tkn, mutability
    VarDecl(Token, Token, bool),

    // Type token, Ident token, mutability, value
    VarAssign(Token, Token, bool, Box<Option<Ast>>),

    // Operator token, lhs ast, rhs ast
    Logical(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, lhs ast, rhs ast
    Binary(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, rhs ast
    Unary(Token, Box<Option<Ast>>),

    // Parenthesized expr
    ParenExpr(Box<Option<Ast>>),

    // Func ident, params, func body
    FnDecl(Token, Vec<Token>, Box<Option<Ast>>),

    // Func name, params
    FnCall(Option<Token>, Vec<Ast>),

    // Class name, class props, class methods
    ClassDecl(Token, Vec<Option<Ast>>, Vec<Option<Ast>>),

    // Name of the class, function/property name
    ClassGet(Option<Token>, Option<Token>),

    // Class name, class prop, rhs ast
    ClassSet(Option<Token>, Option<Token>, Box<Option<Ast>>),

    // Identifier/Literal token
    Primary(Token)
}
