use std::collections::HashMap;
use token::Token;
use ty_rec::TypeRecord;

/// AST represents an AST node in our parse tree. Each node can contain different fields and
/// should be represented by an anonymous struct to better document those fields, so that we know
/// what each of the members of the enum type is supposed to represent.
/// Nodes can be statements or expressions. Expr nodes imply that some value in the node
/// can be "used", ie. that node has a type. Thus, expr nodes should contain a TypeRecord field
/// with type information. Statement nodes generally wrap other statements or expressions,
/// so their containing (child) nodes may have Types but not the statement nodes themselves.
#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Prog {
        stmts: Vec<Ast>,
    },

    BlckStmt {
        stmts: Vec<Ast>,
        sc: usize,
    },

    IfStmt {
        cond_expr: Box<Ast>,
        if_stmts: Box<Ast>,
        elif_exprs: Vec<Ast>,
        el_stmts: Vec<Ast>,
    },

    ElifStmt {
        cond_expr: Box<Ast>,
        stmts: Box<Ast>,
    },

    WhileStmt {
        cond_expr: Box<Ast>,
        stmts: Box<Ast>,
    },

    ForStmt {
        for_var_decl: Box<Ast>,
        for_cond_expr: Box<Ast>,
        for_step_expr: Box<Ast>,
        stmts: Box<Ast>,
    },

    RetStmt {
        ret_expr: Option<Box<Ast>>,
    },

    ExprStmt {
        expr: Box<Ast>,
    },

    VarDeclExpr {
        ty_rec: TypeRecord,
        ident_tkn: Token,
        is_imm: bool,
        is_global: bool,
    },

    VarAssignExpr {
        ty_rec: TypeRecord,
        ident_tkn: Token,
        is_imm: bool,
        is_global: bool,
        value: Box<Ast>,
    },

    LogicalExpr {
        ty_rec: TypeRecord,
        op_tkn: Token,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },

    BinaryExpr {
        ty_rec: TypeRecord,
        op_tkn: Token,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },

    UnaryExpr {
        ty_rec: TypeRecord,
        op_tkn: Token,
        rhs: Box<Ast>,
    },

    PrimaryExpr {
        ty_rec: TypeRecord,
    },

    // TODO: statement or expr
    FnDecl {
        ident_tkn: Token,
        fn_params: Vec<TypeRecord>,
        ret_ty: TypeRecord,
        fn_body: Box<Ast>,
        sc: usize,
    },

    // TODO: statement or expr
    FnCall {
        fn_tkn: Token,
        fn_params: Vec<Ast>,
    },

    // TODO: statement or expr
    ClassDecl {
        ty_rec: TypeRecord,
        ident_tkn: Token,
        methods: Vec<Ast>,
        props: Vec<Ast>,
        prop_pos: HashMap<String, usize>,
        sc: usize,
    },

    // TODO: statement or expr
    ClassPropAccess {
        ident_tkn: Token,
        prop_name: String,
        idx: usize,
        owner_class: Box<Ast>,
    },

    // TODO: statement or expr
    ClassPropSet {
        ident_tkn: Token,
        prop_name: String,
        idx: usize,
        owner_class: Box<Ast>,
        assign_val: Box<Ast>,
    },

    // TODO: statement or expr
    ClassFnCall {
        class_tkn: Token,
        class_name: String,
        fn_tkn: Token,
        fn_params: Vec<Ast>,
        sc: usize,
    },
}

impl Ast {
    pub fn is_primary(&self) -> bool {
        match self {
            Ast::PrimaryExpr { .. } => true,
            _ => false,
        }
    }

    pub fn extract_primary_ty_rec(&self) -> TypeRecord {
        match self {
            Ast::PrimaryExpr { ty_rec } => ty_rec.clone(),
            _ => panic!(),
        }
    }

    pub fn extract_params(&self) -> Vec<Ast> {
        match self {
            Ast::FnCall {
                fn_tkn: _,
                fn_params,
            } => fn_params.clone(),
            _ => Vec::new(),
        }
    }

    pub fn get_ty_rec(&self) -> Option<TypeRecord> {
        match self {
            Ast::PrimaryExpr { ty_rec }
            | Ast::UnaryExpr { ty_rec, .. }
            | Ast::BinaryExpr { ty_rec, .. }
            | Ast::LogicalExpr { ty_rec, .. }
            | Ast::VarAssignExpr { ty_rec, .. }
            | Ast::VarDeclExpr { ty_rec, .. } => Some(ty_rec.clone()),
            _ => None,
        }
    }
}
