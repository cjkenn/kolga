use token::Token;
use type_record::TyRecord;

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Prog(Vec<Ast>),

    // List of statements/expression in a block
    BlckStmt(Vec<Option<Ast>>),

    // Condition expr, if block stmt, else if exprs, else stmts
    IfStmt(Box<Option<Ast>>,
           Box<Option<Ast>>,
           Vec<Option<Ast>>,
           Box<Option<Ast>>
    ),

    // Condition expr, stmts
    ElifStmt(Box<Option<Ast>>, Box<Option<Ast>>),

    // Condition expr, stmts
    WhileStmt(Box<Option<Ast>>, Box<Option<Ast>>),

    // Var decl, condition expr, incr/decr expr, stmts
    ForStmt(Box<Option<Ast>>,
            Box<Option<Ast>>,
            Box<Option<Ast>>,
            Box<Option<Ast>>
    ),

    // Return expr, if any
    RetStmt(Box<Option<Ast>>),

    // expr
    ExprStmt(Box<Option<Ast>>),

    // Var type tkn, var name tkn, mutability
    VarDecl(TyRecord, Token, bool),

    // Type token, Ident token, mutability, value
    VarAssign(TyRecord, Token, bool, Box<Option<Ast>>),

    // Operator token, lhs ast, rhs ast
    Logical(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, lhs ast, rhs ast
    Binary(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, rhs ast
    Unary(Token, Box<Option<Ast>>),

    // Func ident, params, return type record, func body
    FnDecl(Token, Vec<TyRecord>, TyRecord, Box<Option<Ast>>),

    // Func name, params
    FnCall(Option<Token>, Vec<Ast>),

    // Class name, class props, class methods
    ClassDecl(Token, Vec<Option<Ast>>, Vec<Option<Ast>>),

    // Name of the class, function/property name
    ClassGet(Option<Token>, Option<Token>),

    // Class name, class prop, rhs ast
    ClassSet(Option<Token>, Option<Token>, Box<Option<Ast>>),

    // Identifier/Literal token
    Primary(TyRecord)
}

impl Ast {
    pub fn is_primary(&self) -> bool {
        match self {
            Ast::Primary(_) => true,
            _ => false
        }
    }

    pub fn extract_primary_ty_rec(&self) -> TyRecord {
        match self {
            Ast::Primary(tyrec) => tyrec.clone(),
            _ => panic!()
        }
    }
}
