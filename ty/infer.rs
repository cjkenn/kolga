use kolgac::ast::Ast;
use kolgac::token::TknTy;
use kolgac::ty_rec::KolgaTy;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation<'a> {
    pub lhs: KolgaTy,
    pub rhs: KolgaTy,
    ast: &'a Ast,
}

impl<'a> TypeEquation<'a> {
    pub fn new(lhs: KolgaTy, rhs: KolgaTy, ast: &'a Ast) -> TypeEquation<'a> {
        TypeEquation {
            lhs: lhs,
            rhs: rhs,
            ast: ast,
        }
    }
}

pub struct TyInfer {
    subs: HashMap<String, KolgaTy>,
}

impl TyInfer {
    pub fn new() -> TyInfer {
        TyInfer {
            subs: HashMap::new(),
        }
    }

    pub fn infer(&mut self, ast: &mut Ast) -> Result<(), String> {
        match ast {
            Ast::Prog { num: _, stmts } => {
                let ty_eqs = self.ty_eq(stmts);
                self.unify_all(ty_eqs)?;
            }
            _ => panic!("invalid ast found in type infer!"),
        };

        Ok(())
    }

    fn ty_eq<'a>(&self, stmts: &'a mut Vec<Ast>) -> Vec<TypeEquation<'a>> {
        let mut ty_eqs = Vec::new();
        for stmt in stmts.iter() {
            ty_eqs.extend(self.gen_ty_eq(stmt));
        }

        ty_eqs
    }

    fn unify_all<'a>(&mut self, ty_eqs: Vec<TypeEquation<'a>>) -> Result<(), String> {
        for eq in ty_eqs {
            self.unify(eq.lhs, eq.rhs)?;
        }

        Ok(())
    }

    fn unify(&mut self, lhs: KolgaTy, rhs: KolgaTy) -> Result<(), String> {
        if lhs == rhs {
            return Ok(());
        }

        match lhs {
            KolgaTy::Symbolic(_) => {
                return self.unify_var(lhs, rhs);
            }
            _ => (),
        };

        match rhs {
            KolgaTy::Symbolic(_) => {
                return self.unify_var(lhs, rhs);
            }
            _ => (),
        };

        Err(String::from("Could not infer types"))
    }

    // Expect lhs to be KolgaTy::Symbolic
    fn unify_var(&mut self, lhs: KolgaTy, rhs: KolgaTy) -> Result<(), String> {
        let mb_lhs_name = match lhs.clone() {
            KolgaTy::Symbolic(name) => Some(name),
            _ => None,
        };

        let mb_rhs_name = match rhs.clone() {
            KolgaTy::Symbolic(name) => Some(name),
            _ => None,
        };

        let subs_clone = self.subs.clone();

        let name = mb_lhs_name.unwrap();
        if self.subs.contains_key(&name) {
            let existing_ty = subs_clone.get(&name).unwrap();

            return self.unify(existing_ty.clone(), rhs);
        }

        if mb_rhs_name.is_some() && self.subs.contains_key(&mb_rhs_name.clone().unwrap()) {
            let name = mb_rhs_name.unwrap();
            let existing_ty = subs_clone.get(&name).unwrap();

            return self.unify(lhs, existing_ty.clone());
        }

        if self.occurs_check(lhs, rhs.clone()) {
            return Err(String::from(
                "Could not infer types (infinite recursive type found)",
            ));
        }

        self.subs.insert(name, rhs);
        Ok(())
    }

    // Expect lhs to be KolgaTy::Symbolic
    fn occurs_check(&self, lhs: KolgaTy, rhs: KolgaTy) -> bool {
        let mb_rhs_name = match rhs.clone() {
            KolgaTy::Symbolic(name) => Some(name),
            _ => None,
        };

        if lhs == rhs {
            return true;
        }

        let subs_clone = self.subs.clone();

        if mb_rhs_name.is_some() && self.subs.contains_key(&mb_rhs_name.clone().unwrap()) {
            let name = mb_rhs_name.unwrap();
            let existing_ty = subs_clone.get(&name).unwrap();

            return self.occurs_check(lhs, existing_ty.clone());
        }

        false
    }

    fn gen_ty_eq<'a>(&self, ast: &'a Ast) -> Vec<TypeEquation<'a>> {
        let mut ty_eqs = Vec::new();
        match *ast {
            Ast::PrimaryExpr { .. } => ty_eqs,
            Ast::LogicalExpr {
                num: _,
                ref ty_rec,
                ref op_tkn,
                ref lhs,
                ref rhs,
            }
            | Ast::BinaryExpr {
                num: _,
                ref ty_rec,
                ref op_tkn,
                ref lhs,
                ref rhs,
            } => {
                ty_eqs.extend(self.gen_ty_eq(lhs));
                ty_eqs.extend(self.gen_ty_eq(rhs));
                // Binary operators expect numbers as their args: strings are not supported
                // We should be safe to unwrap here, otherwise we have a parsing error
                // (we're trying to put something in an expression without a type)
                let lhs_ty_rec = lhs.get_ty_rec().unwrap();
                let rhs_ty_rec = rhs.get_ty_rec().unwrap();

                ty_eqs.push(TypeEquation::new(lhs_ty_rec.ty, KolgaTy::Num, ast));
                ty_eqs.push(TypeEquation::new(rhs_ty_rec.ty, KolgaTy::Num, ast));

                if op_tkn.ty.is_cmp_op() {
                    ty_eqs.push(TypeEquation::new(ty_rec.ty.clone(), KolgaTy::Bool, ast));
                } else {
                    ty_eqs.push(TypeEquation::new(ty_rec.ty.clone(), KolgaTy::Num, ast));
                }

                ty_eqs
            }
            Ast::UnaryExpr {
                num: _,
                ref ty_rec,
                ref op_tkn,
                ref rhs,
            } => {
                ty_eqs.extend(self.gen_ty_eq(rhs));
                let rhs_ty_rec = rhs.get_ty_rec().unwrap();
                if op_tkn.ty == TknTy::Bang {
                    ty_eqs.push(TypeEquation::new(rhs_ty_rec.ty, KolgaTy::Bool, ast));
                    ty_eqs.push(TypeEquation::new(ty_rec.ty.clone(), KolgaTy::Bool, ast));
                } else {
                    ty_eqs.push(TypeEquation::new(rhs_ty_rec.ty, KolgaTy::Num, ast));
                    ty_eqs.push(TypeEquation::new(ty_rec.ty.clone(), KolgaTy::Num, ast));
                }

                ty_eqs
            }
            Ast::ExprStmt { num: _, ref expr } => {
                ty_eqs.extend(self.gen_ty_eq(expr));
                ty_eqs
            }
            Ast::BlckStmt {
                num: _, ref stmts, ..
            } => {
                for stmt in stmts.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }
                ty_eqs
            }
            Ast::IfStmt {
                num: _,
                ref cond_expr,
                ref if_stmts,
                ref elif_exprs,
                ref el_stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(if_stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(cond_expr_ty_rec.ty, KolgaTy::Bool, ast));

                for stmt in elif_exprs.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }

                for stmt in el_stmts.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }

                ty_eqs
            }
            Ast::ElifStmt {
                num: _,
                ref cond_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(cond_expr_ty_rec.ty, KolgaTy::Bool, ast));

                ty_eqs
            }
            Ast::WhileStmt {
                num: _,
                ref cond_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(cond_expr_ty_rec.ty, KolgaTy::Bool, ast));

                ty_eqs
            }
            Ast::ForStmt {
                num: _,
                ref for_var_decl,
                ref for_cond_expr,
                ref for_step_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                // The var declaration should be a number
                let var_decl_ty_rec = for_var_decl.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(var_decl_ty_rec.ty, KolgaTy::Num, ast));

                // The cond expr should be a bool
                let cond_expr_ty_rec = for_cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(cond_expr_ty_rec.ty, KolgaTy::Bool, ast));

                // The step expression should be a number
                let step_expr_ty_rec = for_step_expr.get_ty_rec().unwrap();
                ty_eqs.push(TypeEquation::new(step_expr_ty_rec.ty, KolgaTy::Num, ast));

                ty_eqs
            }
            Ast::VarAssignExpr {
                num: _,
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
            Ast::RetStmt { .. } => unimplemented!(),
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
