use token::Token;
use type_record::TyRecord;

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Prog {
        stmts: Vec<Ast>
    },

    BlckStmt {
        stmts: Vec<Option<Ast>>,
        scope_lvl: usize
    },

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

    VarDecl {
        ty_rec: TyRecord,
        ident_tkn: Token,
        is_imm: bool
    },

    VarAssign {
        ty_rec: TyRecord,
        ident_tkn: Token,
        is_imm: bool,
        value: Box<Option<Ast>>
    },

    // Operator token, lhs ast, rhs ast
    Logical(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, lhs ast, rhs ast
    Binary(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, rhs ast
    Unary(Token, Box<Option<Ast>>),

    FuncDecl {
        ident_tkn: Token,
        params: Vec<TyRecord>,
        ret_ty: TyRecord,
        func_body: Box<Option<Ast>>,
        scope_lvl: usize
    },

    // Func name, params
    FnCall(Option<Token>, Vec<Ast>),

    // Class name, class methods, class props, symbol scope level
    ClassDecl(Token, Vec<Option<Ast>>, Vec<Option<Ast>>),

    // Name of the class, function/property name
    ClassGet(Option<Token>, Option<Token>),

    // Class name, func name, arguments
    ClassFnCall(Token, Token, Vec<Ast>),

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

    pub fn extract_params(&self) -> Vec<Ast> {
        match self {
            Ast::FnCall(_, pars) => pars.clone(),
            _ => Vec::new()
        }
    }
}
