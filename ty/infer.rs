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
                ref ty_rec,
                ref op_tkn,
                ref lhs,
                ref rhs,
            }
            | Ast::BinaryExpr {
                ref ty_rec,
                ref op_tkn,
                ref lhs,
                ref rhs,
            } => {
                ty_eqs.extend(self.gen_ty_eq(lhs));
                ty_eqs.extend(self.gen_ty_eq(rhs));
                // Binary operators expect numbers as their args: strings are not supported
                // We should be safe to uwnrap here, otherwise we have a parsing error
                // (we're trying to put something in an expression without a type)
                let lhs_ty_rec = lhs.get_ty_rec().unwrap();
                let rhs_ty_rec = rhs.get_ty_rec().unwrap();

                ty_eqs.push(TypeEquation::new(lhs_ty_rec.ty, Some(KolgaTy::Num), ast));
                ty_eqs.push(TypeEquation::new(rhs_ty_rec.ty, Some(KolgaTy::Num), ast));

                if op_tkn.ty.is_cmp_op() {
                    ty_eqs.push(TypeEquation::new(
                        ty_rec.ty.clone(),
                        Some(KolgaTy::Bool),
                        ast,
                    ));
                } else {
                    ty_eqs.push(TypeEquation::new(
                        ty_rec.ty.clone(),
                        Some(KolgaTy::Num),
                        ast,
                    ));
                }

                ty_eqs
            }
            Ast::UnaryExpr {
                ref ty_rec,
                ref op_tkn,
                ref rhs,
            } => {
                ty_eqs.extend(self.gen_ty_eq(rhs));
                let rhs_ty_rec = rhs.get_ty_rec().unwrap();
                if op_tkn.ty == TknTy::Bang {
                    ty_eqs.push(TypeEquation::new(rhs_ty_rec.ty, Some(KolgaTy::Bool), ast));
                    ty_eqs.push(TypeEquation::new(
                        ty_rec.ty.clone(),
                        Some(KolgaTy::Bool),
                        ast,
                    ));
                } else {
                    ty_eqs.push(TypeEquation::new(rhs_ty_rec.ty, Some(KolgaTy::Num), ast));
                    ty_eqs.push(TypeEquation::new(
                        ty_rec.ty.clone(),
                        Some(KolgaTy::Num),
                        ast,
                    ));
                }

                ty_eqs
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
            Ast::IfStmt {
                ref cond_expr,
                ref if_stmts,
                ref elif_exprs,
                ref el_stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(if_stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(
                    cond_expr_ty_rec.ty,
                    Some(KolgaTy::Bool),
                    ast,
                ));

                for stmt in elif_exprs.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }

                for stmt in el_stmts.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }

                ty_eqs
            }
            Ast::ElifStmt {
                ref cond_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(
                    cond_expr_ty_rec.ty,
                    Some(KolgaTy::Bool),
                    ast,
                ));

                ty_eqs
            }
            Ast::WhileStmt {
                ref cond_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(
                    cond_expr_ty_rec.ty,
                    Some(KolgaTy::Bool),
                    ast,
                ));

                ty_eqs
            }
            Ast::ForStmt {
                ref for_var_decl,
                ref for_cond_expr,
                ref for_step_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                // The var declaration should be a number
                let var_decl_ty_rec = for_var_decl.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(
                    var_decl_ty_rec.ty,
                    Some(KolgaTy::Num),
                    ast,
                ));

                // The cond expr should be a bool
                let cond_expr_ty_rec = for_cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(
                    cond_expr_ty_rec.ty,
                    Some(KolgaTy::Bool),
                    ast,
                ));

                // The step expression should be a number
                let step_expr_ty_rec = for_step_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(
                    step_expr_ty_rec.ty,
                    Some(KolgaTy::Num),
                    ast,
                ));

                ty_eqs
            }
            Ast::VarAssignExpr {
                ref ty_rec,
                ident_tkn: _,
                is_imm: _,
                is_global: _,
                ref value,
            } => {
                ty_eqs.extend(self.gen_ty_eq(value));
                let val_ty_rec = value.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(ty_rec.ty.clone(), val_ty_rec.ty, ast));

                ty_eqs
            }
            Ast::VarDeclExpr { .. } => ty_eqs,
            Ast::RetStmt { ret_expr } => {
                if ret_expr.is_some() {
                    ty_eqs.extend(self.gen_ty_eq(&ret_expr.unwrap()));
                }

                ty_eqs
            }
            Ast::ClassDecl { .. } => unimplemented!(),
            Ast::FnDecl { .. } => unimplemented!(),
            Ast::FnCall { .. } => unimplemented!(),
            Ast::ClassPropAccess { .. } => unimplemented!(),
            Ast::ClassPropSet { .. } => unimplemented!(),
            Ast::ClassFnCall { .. } => unimplemented!(),
            _ => ty_eqs,
        }
    }
}
