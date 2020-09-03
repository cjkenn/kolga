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
}

impl<'t> IRGen<'t> {
    pub fn new(ast: &'t Ast) -> IRGen {
        IRGen {
            ast: ast,
            ir: Vec::new(),
        }
    }
}

impl<'t> AstVisitor for IRGen<'t> {
    fn visit_ast(&mut self, node: &Ast) {
        match node {
            Ast::Prog { .. } => walk_prog(self, node),
            _ => {}
        }
    }
}
