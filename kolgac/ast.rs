use std::collections::HashMap;
use token::Token;
use ty_rec::TyRecord;

#[derive(Clone, Debug, PartialEq)]
pub struct MetaAst {
    id: usize,
    line: usize,
    pos: usize,
}

impl MetaAst {
    pub fn new(id: usize, line: usize, pos: usize) -> MetaAst {
        MetaAst {
            id: id,
            line: line,
            pos: pos,
        }
    }
}

/// AST represents an AST node in our parse tree. Each node can contain different fields and
/// should be represented by an anonymous struct to better document those fields, so that we know
/// what each of the members of the enum type is supposed to represent.
/// Nodes can be statements or expressions. Expr nodes imply that some value in the node
/// can be "used", ie. that node has a type. Thus, expr nodes should contain a TyRecord field
/// with type information. Statement nodes generally wrap other statements or expressions,
/// so their containing (child) nodes may have Types but not the statement nodes themselves.
/// The num field of an AST node is suitable to use as an identifier for a block.
#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Prog {
        meta: MetaAst,
        stmts: Vec<Ast>,
    },

    BlckStmt {
        meta: MetaAst,
        stmts: Vec<Ast>,
        sc: usize,
    },

    IfStmt {
        meta: MetaAst,
        cond_expr: Box<Ast>,
        if_stmts: Box<Ast>,
        elif_exprs: Vec<Ast>,
        el_stmts: Vec<Ast>,
    },

    ElifStmt {
        meta: MetaAst,
        cond_expr: Box<Ast>,
        stmts: Box<Ast>,
    },

    WhileStmt {
        meta: MetaAst,
        cond_expr: Box<Ast>,
        stmts: Box<Ast>,
    },

    ForStmt {
        meta: MetaAst,
        for_var_decl: Box<Ast>,
        for_cond_expr: Box<Ast>,
        for_step_expr: Box<Ast>,
        stmts: Box<Ast>,
    },

    RetStmt {
        meta: MetaAst,
        ret_expr: Option<Box<Ast>>,
    },

    ExprStmt {
        meta: MetaAst,
        expr: Box<Ast>,
    },

    VarDeclExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
        ident_tkn: Token,
        is_imm: bool,
        is_global: bool,
    },

    VarAssignExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
        ident_tkn: Token,
        is_imm: bool,
        is_global: bool,
        value: Box<Ast>,
    },

    LogicalExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
        op_tkn: Token,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },

    BinaryExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
        op_tkn: Token,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },

    UnaryExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
        op_tkn: Token,
        rhs: Box<Ast>,
    },

    PrimaryExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
    },

    FnDeclStmt {
        meta: MetaAst,
        ident_tkn: Token,
        fn_params: Vec<TyRecord>,
        ret_ty: TyRecord,
        fn_body: Box<Ast>,
        sc: usize,
    },

    FnCallExpr {
        meta: MetaAst,
        ty_rec: TyRecord,
        fn_tkn: Token,
        fn_params: Vec<Ast>,
    },

    // TODO: statement or expr
    ClassDecl {
        meta: MetaAst,
        ty_rec: TyRecord,
        ident_tkn: Token,
        methods: Vec<Ast>,
        props: Vec<Ast>,
        prop_pos: HashMap<String, usize>,
        sc: usize,
    },

    // TODO: statement or expr
    ClassPropAccess {
        meta: MetaAst,
        ident_tkn: Token,
        prop_name: String,
        idx: usize,
        owner_class: Box<Ast>,
    },

    // TODO: statement or expr
    ClassPropSet {
        meta: MetaAst,
        ident_tkn: Token,
        prop_name: String,
        idx: usize,
        owner_class: Box<Ast>,
        assign_val: Box<Ast>,
    },

    ClassFnCallExpr {
        meta: MetaAst,
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

    pub fn extract_params(&self) -> Vec<Ast> {
        match self {
            Ast::FnCallExpr {
                meta: _,
                ty_rec: _,
                fn_tkn: _,
                fn_params,
            } => fn_params.clone(),
            _ => Vec::new(),
        }
    }

    pub fn get_ty_rec(&self) -> Option<TyRecord> {
        match self {
            Ast::PrimaryExpr { meta: _, ty_rec }
            | Ast::UnaryExpr {
                meta: _, ty_rec, ..
            }
            | Ast::BinaryExpr {
                meta: _, ty_rec, ..
            }
            | Ast::LogicalExpr {
                meta: _, ty_rec, ..
            }
            | Ast::VarAssignExpr {
                meta: _, ty_rec, ..
            }
            | Ast::VarDeclExpr {
                meta: _, ty_rec, ..
            }
            | Ast::FnCallExpr {
                meta: _, ty_rec, ..
            } => Some(ty_rec.clone()),
            _ => None,
        }
    }
}
