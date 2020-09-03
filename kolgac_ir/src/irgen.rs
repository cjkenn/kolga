use crate::instr::Instr;

use kolgac::{ast::Ast, visit::*};

#[derive(Debug)]
pub struct IRGen<'t> {
    /// Syntax tree obtained from parsing. This is assumed to
    /// be valid at this stage.
    pub ast: &'t Ast,

    /// The resulting three address linear code. This is represented
    /// as a vector of instructions.
    pub ir: Vec<Instr>,

    reg: usize,
}

impl<'t> IRGen<'t> {
    pub fn new(ast: &'t Ast) -> IRGen {
        IRGen {
            ast: ast,
            ir: Vec::new(),
            reg: 0,
        }
    }

    fn reg_name(&self) -> String {
        format!("r{}", self.reg)
    }

    fn next_reg(&mut self) {
        // TOOD: gets more complex with reg constraints.
        self.reg = self.reg + 1
    }
}

impl<'t> AstVisitor for IRGen<'t> {
    fn visit_ast(&mut self, node: &Ast) {
        match node {
            Ast::Prog { .. } => walk_prog(self, node),
            Ast::BlckStmt { .. } => walk_blck_stmt(self, node),
            Ast::BinaryExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                lhs,
                rhs,
            } => {
                // 4 + x
                // r0 = mv 4
                // r1 = ld x
                // r2 = r0 + r1
            }
            Ast::ExprStmt { .. } => walk_expr_stmt(self, node),
            Ast::PrimaryExpr { .. } => walk_primary(self, node),
            _ => {}
        }
    }
}
