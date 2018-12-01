use error::ty::TypeErr;
use kolgac::ast::Ast;
use kolgac::token::TknTy;
use kolgac::ty_rec::KolgaTy;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation<'a> {
    pub lhs: Option<KolgaTy>,
    pub rhs: Option<KolgaTy>,
    ast: &'a Ast,
}

impl<'a> TypeEquation<'a> {
    pub fn new(lhs: Option<KolgaTy>, rhs: Option<KolgaTy>, ast: &'a Ast) -> TypeEquation<'a> {
        TypeEquation {
            lhs: lhs,
            rhs: rhs,
            ast: ast,
        }
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
                let ty_eqs = self.ty_eq(stmts);
                // TODO: unify should return a map from var name to TypeRecord. We should then
                // iterate this map and set all the TypeRecordords in the symbol table to these
                // values in the map. The symbol table and ast ty records need to be kept
                // in sync though
                // self.unify(stmts);
            }
            _ => panic!("invalid ast found in type infer!"),
        };

        self.errors.clone()
    }

    fn ty_eq<'a>(&self, stmts: &'a mut Vec<Ast>) -> Vec<TypeEquation<'a>> {
        let mut ty_eqs = Vec::new();
        for stmt in stmts.iter() {
            ty_eqs.extend(self.gen_ty_eq(stmt));
        }

        ty_eqs
    }

    fn unify(&mut self, stmts: &mut Vec<Ast>) {
        unimplemented!()
    }

    fn gen_ty_eq<'a>(&self, ast: &'a Ast) -> Vec<TypeEquation<'a>> {
        let mut ty_eqs = Vec::new();
        match *ast {
            Ast::PrimaryExpr { ref ty_rec } => {
                let lhs_ty = match ty_rec.tkn.ty {
                    TknTy::Num => Some(KolgaTy::Num),
                    TknTy::String => Some(KolgaTy::String),
                    TknTy::Str(_) => Some(KolgaTy::String),
                    TknTy::Val(_) => Some(KolgaTy::Num),
                    TknTy::Bool => Some(KolgaTy::Bool),
                    TknTy::True | TknTy::False => Some(KolgaTy::Bool),
                    TknTy::Minus => Some(KolgaTy::Num),
                    TknTy::Bang => Some(KolgaTy::Bool),
                    TknTy::Void => Some(KolgaTy::Void),
                    _ => None,
                };

                let rhs_ty = ty_rec.ty.clone();
                ty_eqs.push(TypeEquation::new(lhs_ty, rhs_ty, ast));
                ty_eqs
            }
            Ast::LogicalExpr {
                ref mut ty_rec,
                op_tkn,
                ref mut lhs,
                ref mut rhs,
            }
            | Ast::BinaryExpr {
                ref mut ty_rec,
                op_tkn,
                ref mut lhs,
                ref mut rhs,
            } => {
                ty_eqs.extend(self.gen_ty_eq(lhs));
                ty_eqs.extend(self.gen_ty_eq(rhs));
            }
            Ast::UnaryExpr {
                ref mut ty_rec,
                op_tkn: _,
                ref mut rhs,
            } => {
                self.gen_ty_eq(rhs);
            }
            Ast::ExprStmt { ref expr } => {
                ty_eqs.extend(self.gen_ty_eq(expr));
                ty_eqs
            }
            Ast::BlckStmt { ref stmts, .. } => {
                for stmt in stmts.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }
                ty_eqs
            }
            //     Ast::IfStmt {
        //         ref mut cond_expr,
        //         ref mut if_stmts,
        //         ref mut elif_exprs,
        //         ref mut el_stmts,
        //     } => {
        //         self.gen_ty_eq(cond_expr);
        //         self.gen_ty_eq(if_stmts);

        //         for stmt in elif_exprs.iter_mut() {
        //             self.gen_ty_eq(stmt);
        //         }

        //         for stmt in el_stmts.iter_mut() {
        //             self.gen_ty_eq(stmt);
        //         }
        //     }
        //     Ast::ElifStmt {
        //         ref mut cond_expr,
        //         ref mut stmts,
        //     } => {
        //         self.gen_ty_eq(cond_expr);
        //         self.gen_ty_eq(stmts);
        //     }
        //     Ast::ForStmt {
        //         ref mut for_var_decl,
        //         ref mut for_cond_expr,
        //         ref mut for_step_expr,
        //         ref mut stmts,
        //     } => {
        //         self.gen_ty_eq(for_var_decl);
        //         self.gen_ty_eq(for_cond_expr);
        //         self.gen_ty_eq(for_step_expr);
        //         self.gen_ty_eq(stmts);
        //     }
        //     Ast::WhileStmt {
        //         ref mut cond_expr,
        //         ref mut stmts,
        //     } => {
        //         self.gen_ty_eq(cond_expr);
        //         self.gen_ty_eq(stmts);
        //     }

        //     Ast::VarAssignExpr {
        //         ref mut ty_rec,
        //         ident_tkn: _,
        //         is_imm: _,
        //         is_global: _,
        //         ref mut value,
        //     } => {
        //         self.gen_ty_eq(value);
        //     }
            // Ast::VarDeclExpr { ref ty_rec, .. } => {}

            // Ast::ClassDecl { .. } => unimplemented!(),
            _ => ty_eqs,
        }
    }
}
