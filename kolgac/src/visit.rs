use crate::ast::Ast;

pub trait AstVisitor {
    fn visit_ast(&mut self, node: &Ast);
}

pub fn walk_prog<V: AstVisitor>(visitor: &mut V, node: &Ast) {
    match node {
        Ast::Prog { meta: _, stmts } => {
            for stmt in stmts {
                visitor.visit_ast(stmt);
            }
        }
        _ => {}
    }
}

pub fn walk_blck_stmt<V: AstVisitor>(visitor: &mut V, node: &Ast) {
    match node {
        Ast::BlckStmt { meta: _, stmts, .. } => {
            for stmt in stmts {
                visitor.visit_ast(stmt);
            }
        }
        _ => {}
    }
}

pub fn walk_expr_stmt<V: AstVisitor>(visitor: &mut V, node: &Ast) {
    match node {
        Ast::ExprStmt { meta: _, expr } => {
            visitor.visit_ast(expr);
        }
        _ => {}
    }
}

pub fn walk_primary<V: AstVisitor>(_visitor: &mut V, _node: &Ast) {
    // empty walk
}
