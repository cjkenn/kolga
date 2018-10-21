use token::Token;
use ty_rec::TyRec;

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Prog {
        stmts: Vec<Ast>
    },

    BlckStmt {
        stmts: Vec<Option<Ast>>,
        sc: usize
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

    ForStmt{
        for_var_decl: Box<Option<Ast>>,
        for_cond_expr: Box<Option<Ast>>,
        for_step_expr: Box<Option<Ast>>,
        stmts: Box<Option<Ast>>
    },

    // Return expr, if any
    RetStmt(Box<Option<Ast>>),

    // expr
    ExprStmt(Box<Option<Ast>>),

    VarDecl {
        ty_rec: TyRec,
        ident_tkn: Token,
        is_imm: bool,
        is_global: bool
    },

    VarAssign {
        ty_rec: TyRec,
        ident_tkn: Token,
        is_imm: bool,
        is_global: bool,
        value: Box<Option<Ast>>
    },

    // Operator token, lhs ast, rhs ast
    Logical(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, lhs ast, rhs ast
    Binary(Token, Box<Option<Ast>>, Box<Option<Ast>>),

    // Operator token, rhs ast
    Unary(Token, Box<Option<Ast>>),

    FnDecl {
        ident_tkn: Token,
        fn_params: Vec<TyRec>,
        ret_ty: TyRec,
        fn_body: Box<Option<Ast>>,
        sc: usize
    },

    // Func name, params
    FnCall {
        fn_tkn: Token,
        fn_params: Vec<Ast>
    },

    ClassDecl {
        ident_tkn: Token,
        methods: Vec<Option<Ast>>,
        props: Vec<Option<Ast>>,
        sc: usize
    },

    ClassGet {
        class_tkn: Token,
        prop_tkn: Token
    },

    ClassSet {
        class_tkn: Token,
        prop_tkn: Token,
        assign_val: Box<Option<Ast>>
    },

    ClassFnCall {
        class_tkn: Token,
        class_name: String,
        fn_tkn: Token,
        fn_params: Vec<Ast>,
        sc: usize
    },

    // Identifier/Literal token
    Primary(TyRec)
}

impl Ast {
    pub fn is_primary(&self) -> bool {
        match self {
            Ast::Primary(_) => true,
            _ => false
        }
    }

    pub fn extract_primary_ty_rec(&self) -> TyRec {
        match self {
            Ast::Primary(tyrec) => tyrec.clone(),
            _ => panic!()
        }
    }

    pub fn extract_params(&self) -> Vec<Ast> {
        match self {
            Ast::FnCall{fn_tkn:_, fn_params} => fn_params.clone(),
            _ => Vec::new()
        }
    }
}
