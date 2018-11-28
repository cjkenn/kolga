use kolgac::ast::Ast;
use kolgac::ty_rec::{TyName, TyRec};
use error::ty::TypeErr;

pub struct TyInfer {
    ty_count: usize,
    errors: Vec<TypeErr>
}

impl TyInfer {
    pub fn new() -> TyInfer {
        TyInfer {
            ty_count: 0,
            errors: Vec::new()
        }
    }

    pub fn infer(&mut self, ast: &mut Ast) -> Vec<TypeErr> {
        match ast {
            Ast::Prog{stmts} => {
                self.assign(stmts);
                //self.gen_eq(stmts);
                //self.unify(stmts);
            },
            _ => panic!("invalid ast found in type infer!")
        };

        self.errors.clone()
    }

    fn assign(&mut self, stmts: &mut Vec<Ast>) {
        for stmt in stmts.iter_mut() {
            self.assign_ast(stmt);
        }
    }

    fn gen_eq(&mut self, stmts: &mut Vec<Ast>) {
        unimplemented!()
    }

    fn unify(&mut self, stmts: &mut Vec<Ast>) {
        unimplemented!()
    }

    fn assign_ast(&mut self, ast: &mut Ast) {
        match *ast {
            Ast::ExprStmt(ref mut ast) => self.assign_ast(ast),
            Ast::BlckStmt{ref mut stmts, ..} => {
                for stmt in stmts.iter_mut() {
                    self.assign_ast(stmt);
                }
            },
            Ast::IfStmt{ ref mut cond_expr,
                         ref mut if_stmts,
                         ref mut elif_exprs,
                         ref mut el_stmts } => {
                self.assign_ast(cond_expr);
                self.assign_ast(if_stmts);

                for stmt in elif_exprs.iter_mut() {
                    self.assign_ast(stmt);
                }

                for stmt in el_stmts.iter_mut() {
                    self.assign_ast(stmt);
                }
            },
            Ast::LogicalExpr{ ref mut ty_rec, op_tkn:_, ref mut lhs, ref mut rhs } |
            Ast::BinaryExpr{ ref mut ty_rec, op_tkn:_, ref mut lhs, ref mut rhs } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
                self.assign_ast(lhs);
                self.assign_ast(rhs);
            },
            Ast::UnaryExpr{ ref mut ty_rec, op_tkn:_, ref mut rhs } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
                self.assign_ast(rhs);
            },
            Ast::VarAssignExpr{ ref mut ty_rec,
                                ident_tkn:_,
                                is_imm:_,
                                is_global:_,
                                ref mut value } => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
                self.assign_ast(value);
            },
            Ast::VarDeclExpr{ref mut ty_rec, ..}   |
            Ast::PrimaryExpr{ref mut ty_rec} => {
                ty_rec.ty = Some(TyName::Symbolic(self.curr_symbolic_ty()));
            },
            _ => ()
        }
    }

    fn set_ty(&mut self, ty_rec: &mut TyRec) {
        let new_ty = self.curr_symbolic_ty();
        ty_rec.update(Some(TyName::Symbolic(new_ty)));
    }

    fn curr_symbolic_ty(&mut self) -> String {
        self.ty_count = self.ty_count + 1;
        format!("T{}", self.ty_count)
    }
}
