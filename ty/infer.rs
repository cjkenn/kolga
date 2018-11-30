use error::ty::TypeErr;
use kolgac::ast::Ast;
use kolgac::ty_rec::{TyName, TyRec};

struct TyEq {
    pub lhs: TyRec,
    pub rhs: TyRec,
}

impl TyEq {
    pub fn new(lhs: TyRec, rhs: TyRec) -> TyEq {
        TyEq { lhs: lhs, rhs: rhs }
    }
}

pub struct TyInfer {
    ty_count: usize,
    errors: Vec<TypeErr>,
}

impl TyInfer {
    pub fn new() -> TyInfer {
        TyInfer {
            ty_count: 0,
            errors: Vec::new(),
        }
    }

    pub fn infer(&mut self, ast: &mut Ast) -> Vec<TypeErr> {
        match ast {
            Ast::Prog { stmts } => {
                self.assign(stmts);
                let ty_eqs = self.ty_eq(stmts);
                // TODO: unify should return a map from var name to TyRec. We should then
                // iterate this map and set all the TyRecords in the symbol table to these
                // values in the map. The symbol table and ast ty records need to be kept
                // in sync though
                // self.unify(stmts);
            }
            _ => panic!("invalid ast found in type infer!"),
        };

        self.errors.clone()
    }

    fn assign(&mut self, stmts: &mut Vec<Ast>) {
        for stmt in stmts.iter_mut() {
            self.assign_ast(stmt);
        }
    }

    fn ty_eq(&mut self, stmts: &mut Vec<Ast>) -> Vec<TyEq> {
        let mut ty_eqs = Vec::new();
        for stmt in stmts.iter() {
            ty_eqs.push(self.generate_ty_eq(stmt));
        }

        ty_eqs
    }

    fn unify(&mut self, stmts: &mut Vec<Ast>) {
        unimplemented!()
    }

    fn assign_ast(&mut self, ast: &mut Ast) {
        match *ast {
            Ast::ExprStmt(ref mut ast) => self.assign_ast(ast),
            Ast::BlckStmt { ref mut stmts, .. } => {
                for stmt in stmts.iter_mut() {
                    self.assign_ast(stmt);
                }
            }
            Ast::IfStmt {
                ref mut cond_expr,
                ref mut if_stmts,
                ref mut elif_exprs,
                ref mut el_stmts,
            } => {
                self.assign_ast(cond_expr);
                self.assign_ast(if_stmts);

                for stmt in elif_exprs.iter_mut() {
                    self.assign_ast(stmt);
                }

                for stmt in el_stmts.iter_mut() {
                    self.assign_ast(stmt);
                }
            }
            Ast::ElifStmt {
                ref mut cond_expr,
                ref mut stmts,
            } => {
                self.assign_ast(cond_expr);
                self.assign_ast(stmts);
            }
            Ast::ForStmt {
                ref mut for_var_decl,
                ref mut for_cond_expr,
                ref mut for_step_expr,
                ref mut stmts,
            } => {
                self.assign_ast(for_var_decl);
                self.assign_ast(for_cond_expr);
                self.assign_ast(for_step_expr);
                self.assign_ast(stmts);
            }
            Ast::WhileStmt {
                ref mut cond_expr,
                ref mut stmts,
            } => {
                self.assign_ast(cond_expr);
                self.assign_ast(stmts);
            }
            Ast::LogicalExpr {
                ref mut ty_rec,
                op_tkn: _,
                ref mut lhs,
                ref mut rhs,
            }
            | Ast::BinaryExpr {
                ref mut ty_rec,
                op_tkn: _,
                ref mut lhs,
                ref mut rhs,
            } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
                self.assign_ast(lhs);
                self.assign_ast(rhs);
            }
            Ast::UnaryExpr {
                ref mut ty_rec,
                op_tkn: _,
                ref mut rhs,
            } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
                self.assign_ast(rhs);
            }
            Ast::VarAssignExpr {
                ref mut ty_rec,
                ident_tkn: _,
                is_imm: _,
                is_global: _,
                ref mut value,
            } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
                self.assign_ast(value);
            }
            Ast::VarDeclExpr { ref mut ty_rec, .. } | Ast::PrimaryExpr { ref mut ty_rec } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
            }
            Ast::ClassDecl { .. } => unimplemented!(),
            _ => (),
        }
    }

    fn generate_ty_eq(&self, stmt: &Ast) -> TyEq {}

    fn set_ty(&mut self, ty_rec: &mut TyRec) {
        let new_ty = self.curr_symbolic_ty();
        ty_rec.update(Some(TyName::Symbolic(new_ty)));
    }

    fn curr_symbolic_ty(&mut self) -> String {
        self.ty_count = self.ty_count + 1;
        format!("T{}", self.ty_count)
    }
}
