use kolgac::ast::Ast;
use kolgac::token::TknTy;
use kolgac::ty_rec::KolgaTy;
use std::collections::HashMap;

/// Represents an pair of types that can be unified. It's possible that the types
/// in the pair are already the same, in which case unification isn't required.
#[derive(Clone, Debug, PartialEq)]
pub struct TyMatch {
    pub lhs: KolgaTy,
    pub rhs: KolgaTy,
}

impl TyMatch {
    pub fn new(lhs: KolgaTy, rhs: KolgaTy) -> TyMatch {
        TyMatch { lhs: lhs, rhs: rhs }
    }
}

/// Used to infer types for a given AST.
pub struct TyInfer {
    subs: HashMap<String, KolgaTy>,
}

impl TyInfer {
    pub fn new() -> TyInfer {
        TyInfer {
            subs: HashMap::new(),
        }
    }

    /// Infers any types for a given AST. This function will walk the entire
    /// AST twice:
    ///
    /// 1. The first pass is to generate TyMatch structs, which contain a pair
    ///    of KolgTy's to be unified.
    /// 2. Next, we unify all the pairs of types in our TyMatch structs, and generate
    ///    a map from typename to the most general unified type.
    /// 3. In the second pass of the AST, we replace all instances of symbolic types
    ///    in the AST with the mgu's contained in the type mapping. After this pass,
    ///    our program should have no symbolic types remaining.
    ///
    /// Returns an empty result, indication success. There is no result to return,
    /// as we alter the AST in place in the last step of the function.
    pub fn infer(&mut self, ast: &mut Ast) -> Result<(), String> {
        match ast {
            Ast::Prog { meta: _, stmts } => {
                let ty_eqs = self.ty_eq(stmts);
                self.unify_all(ty_eqs)?;
            }
            _ => return Err(String::from("Invalid AST found in infer")),
        };

        match ast {
            Ast::Prog { meta: _, stmts } => {
                for stmt in stmts.iter_mut() {
                    self.update_tys(stmt);
                }
            }
            _ => (),
        };

        Ok(())
    }

    /// Rewrites the type records in the passed in AST. After unification, we
    /// have a mappping from type name to the unified type, so we need to alter
    /// the existing type record to change the type to that. This function
    /// should walk the entire AST.
    fn update_tys(&self, ast: &mut Ast) {
        match *ast {
            Ast::BlckStmt {
                meta: _,
                ref mut stmts,
                ..
            } => {
                for stmt in stmts.iter_mut() {
                    self.update_tys(stmt);
                }
            }
            Ast::IfStmt {
                meta: _,
                ref mut cond_expr,
                ref mut if_stmts,
                ref mut elif_exprs,
                ref mut el_stmts,
            } => {
                self.update_tys(cond_expr);
                self.update_tys(if_stmts);

                for stmt in elif_exprs.iter_mut() {
                    self.update_tys(stmt);
                }

                for stmt in el_stmts.iter_mut() {
                    self.update_tys(stmt);
                }
            }
            Ast::ElifStmt {
                meta: _,
                ref mut cond_expr,
                ref mut stmts,
            } => {
                self.update_tys(cond_expr);
                self.update_tys(stmts);
            }
            Ast::WhileStmt {
                meta: _,
                ref mut cond_expr,
                ref mut stmts,
            } => {
                self.update_tys(cond_expr);
                self.update_tys(stmts);
            }
            Ast::ForStmt {
                meta: _,
                ref mut for_var_decl,
                ref mut for_cond_expr,
                ref mut for_step_expr,
                ref mut stmts,
            } => {
                self.update_tys(for_var_decl);
                self.update_tys(for_cond_expr);
                self.update_tys(for_step_expr);
                self.update_tys(stmts);
            }
            Ast::ExprStmt {
                meta: _,
                ref mut expr,
            } => {
                self.update_tys(expr);
            }
            Ast::VarAssignExpr {
                meta: _,
                ref mut ty_rec,
                ident_tkn: _,
                is_imm: _,
                is_global: _,
                ref mut value,
            } => {
                let potential_ty = self.subs.get(&ty_rec.name);
                if potential_ty.is_some() {
                    ty_rec.ty = potential_ty.unwrap().clone();
                    self.update_tys(value);
                }
            }
            Ast::LogicalExpr {
                meta: _,
                ref mut ty_rec,
                op_tkn: _,
                ref mut lhs,
                ref mut rhs,
            }
            | Ast::BinaryExpr {
                meta: _,
                ref mut ty_rec,
                op_tkn: _,
                ref mut lhs,
                ref mut rhs,
            } => {
                let potential_ty = self.subs.get(&ty_rec.name);
                if potential_ty.is_some() {
                    ty_rec.ty = potential_ty.unwrap().clone();
                    self.update_tys(lhs);
                    self.update_tys(rhs);
                }
            }
            Ast::UnaryExpr {
                meta: _,
                ref mut ty_rec,
                op_tkn: _,
                ref mut rhs,
            } => {
                let potential_ty = self.subs.get(&ty_rec.name);
                if potential_ty.is_some() {
                    ty_rec.ty = potential_ty.unwrap().clone();
                    self.update_tys(rhs);
                }
            }
            Ast::VarDeclExpr {
                meta: _,
                ref mut ty_rec,
                ..
            }
            | Ast::PrimaryExpr {
                meta: _,
                ref mut ty_rec,
                ..
            } => {
                let potential_ty = self.subs.get(&ty_rec.name);
                if potential_ty.is_some() {
                    ty_rec.ty = potential_ty.unwrap().clone();
                }
            }
            Ast::FnDeclStmt {
                meta: _,
                ident_tkn: _,
                fn_params: _,
                ret_ty: _,
                ref mut fn_body,
                ..
            } => {
                self.update_tys(fn_body);
            }
            Ast::RetStmt {
                meta: _,
                ref mut ret_expr,
            } => {
                match *ret_expr {
                    Some(ref mut expr) => self.update_tys(expr),
                    None => (),
                };
            }
            Ast::ClassDecl { .. } => unimplemented!(),
            Ast::FnCallExpr { .. } => unimplemented!(),
            Ast::ClassPropAccess { .. } => unimplemented!(),
            Ast::ClassPropSet { .. } => unimplemented!(),
            Ast::ClassFnCallExpr { .. } => unimplemented!(),
            Ast::Prog { .. } => (),
        }
    }

    fn ty_eq(&self, stmts: &mut Vec<Ast>) -> Vec<TyMatch> {
        let mut ty_eqs = Vec::new();
        for stmt in stmts.iter() {
            ty_eqs.extend(self.gen_ty_eq(stmt));
        }

        ty_eqs
    }

    fn unify_all(&mut self, ty_eqs: Vec<TyMatch>) -> Result<(), String> {
        for eq in ty_eqs {
            self.unify(eq.lhs, eq.rhs)?;
        }

        Ok(())
    }

    /// Unifies two arbitrary types. At least one of the provided types
    /// should be a symbolic type, so long as the types aren't the same.
    /// This prevents an attempt at trying to unify two concrete types,
    /// like String and Num, which can never be unified.
    ///
    /// When we don't have at least one symbolic type here, it should indicate
    /// that we are trying to assign something to the wrong type. For example,
    /// take this program:
    ///
    /// let x ~= 10;
    /// x = "hello";
    ///
    /// When attempting to unify this program, we would end up with an lhs
    /// argument Num, and a rhs arg String, which we cannot unify. In this case,
    /// we should return a type error with a type mismatch.
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
                return self.unify_var(rhs, lhs);
            }
            _ => (),
        };

        // TODO: line numbers would be much better here
        Err(format!(
            "kolgac: Type error - Cannot assign type '{}' to type '{}'",
            rhs, lhs
        ))
    }

    /// Unifies two variable types. This is done by inserting the type on the rhs
    /// into our type mapping under the key provided by the lhs name. However,
    /// this is only done after we recursively call unify on the provided types,
    /// which we do to ensure that if we have already unified a pair, that unification
    /// is honored throughout the entire unification process.
    /// We Expect lhs to be KolgaTy::Symbolic
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

        // Recursively unify if we have already tried to unify the lhs type,
        // to continue honoring the association between the exiting lhs type
        // and the provided rhs type.
        let name = mb_lhs_name.unwrap();
        if self.subs.contains_key(&name) {
            let existing_ty = subs_clone.get(&name).unwrap();

            return self.unify(existing_ty.clone(), rhs);
        }

        // Do the same for the rhs type.
        if mb_rhs_name.is_some() && self.subs.contains_key(&mb_rhs_name.clone().unwrap()) {
            let name = mb_rhs_name.unwrap();
            let existing_ty = subs_clone.get(&name).unwrap();

            return self.unify(lhs, existing_ty.clone());
        }

        // Ensure that the type doesn't contain a reference to itself
        // (ie. let x = x) to prevent infinite unification.
        if self.occurs_check(lhs, rhs.clone()) {
            return Err(String::from(
                "Could not infer types (infinite recursive type found)",
            ));
        }

        // Insert the unified type for the lhs key (the name of the symbolic type)
        self.subs.insert(name, rhs);
        Ok(())
    }

    /// Checks if the provided lhs type occurs "inside" of the provided rhs type.
    /// This check is needed to avoid infinite recursion during unification
    /// (we would endlessly try to unify a type within itself).
    /// We expect lhs to be KolgaTy::Symbolic
    fn occurs_check(&self, lhs: KolgaTy, rhs: KolgaTy) -> bool {
        let mb_rhs_name = match rhs.clone() {
            KolgaTy::Symbolic(name) => Some(name),
            _ => None,
        };

        if lhs == rhs {
            return true;
        }

        let subs_clone = self.subs.clone();

        // We check if the rhs type is in our type mapping. If it is, we've already
        // recorded a type for the symbolic type provided. In that case, we need to
        // recursively check that type as well.
        if mb_rhs_name.is_some() && self.subs.contains_key(&mb_rhs_name.clone().unwrap()) {
            let name = mb_rhs_name.unwrap();
            let existing_ty = subs_clone.get(&name).unwrap();

            return self.occurs_check(lhs, existing_ty.clone());
        }

        false
    }

    /// Walks the entire AST and creates pairs of KolgaTy's to be unified in the next
    /// step of type inference. Typing rules are applied in this step to determine
    /// which types we expect certain expressions to evaluate to.
    fn gen_ty_eq(&self, ast: &Ast) -> Vec<TyMatch> {
        let mut ty_eqs = Vec::new();
        match *ast {
            Ast::PrimaryExpr { .. } => ty_eqs,
            Ast::LogicalExpr {
                meta: _,
                ref ty_rec,
                ref op_tkn,
                ref lhs,
                ref rhs,
            }
            | Ast::BinaryExpr {
                meta: _,
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

                ty_eqs.push(TyMatch::new(lhs_ty_rec.ty, KolgaTy::Num));
                ty_eqs.push(TyMatch::new(rhs_ty_rec.ty, KolgaTy::Num));

                if op_tkn.ty.is_cmp_op() {
                    ty_eqs.push(TyMatch::new(ty_rec.ty.clone(), KolgaTy::Bool));
                } else {
                    ty_eqs.push(TyMatch::new(ty_rec.ty.clone(), KolgaTy::Num));
                }

                ty_eqs
            }
            Ast::UnaryExpr {
                meta: _,
                ref ty_rec,
                ref op_tkn,
                ref rhs,
            } => {
                ty_eqs.extend(self.gen_ty_eq(rhs));
                let rhs_ty_rec = rhs.get_ty_rec().unwrap();
                if op_tkn.ty == TknTy::Bang {
                    ty_eqs.push(TyMatch::new(rhs_ty_rec.ty, KolgaTy::Bool));
                    ty_eqs.push(TyMatch::new(ty_rec.ty.clone(), KolgaTy::Bool));
                } else {
                    ty_eqs.push(TyMatch::new(rhs_ty_rec.ty, KolgaTy::Num));
                    ty_eqs.push(TyMatch::new(ty_rec.ty.clone(), KolgaTy::Num));
                }

                ty_eqs
            }
            Ast::ExprStmt { meta: _, ref expr } => {
                ty_eqs.extend(self.gen_ty_eq(expr));
                ty_eqs
            }
            Ast::BlckStmt {
                meta: _, ref stmts, ..
            } => {
                for stmt in stmts.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }
                ty_eqs
            }
            Ast::IfStmt {
                meta: _,
                ref cond_expr,
                ref if_stmts,
                ref elif_exprs,
                ref el_stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(if_stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TyMatch::new(cond_expr_ty_rec.ty, KolgaTy::Bool));

                for stmt in elif_exprs.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }

                for stmt in el_stmts.iter() {
                    ty_eqs.extend(self.gen_ty_eq(stmt));
                }

                ty_eqs
            }
            Ast::ElifStmt {
                meta: _,
                ref cond_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TyMatch::new(cond_expr_ty_rec.ty, KolgaTy::Bool));

                ty_eqs
            }
            Ast::WhileStmt {
                meta: _,
                ref cond_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                let cond_expr_ty_rec = cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TyMatch::new(cond_expr_ty_rec.ty, KolgaTy::Bool));

                ty_eqs
            }
            Ast::ForStmt {
                meta: _,
                ref for_var_decl,
                ref for_cond_expr,
                ref for_step_expr,
                ref stmts,
            } => {
                ty_eqs.extend(self.gen_ty_eq(stmts));

                // The var declaration should be a number
                let var_decl_ty_rec = for_var_decl.get_ty_rec().unwrap();
                ty_eqs.push(TyMatch::new(var_decl_ty_rec.ty, KolgaTy::Num));

                // The cond expr should be a bool
                let cond_expr_ty_rec = for_cond_expr.get_ty_rec().unwrap();
                ty_eqs.push(TyMatch::new(cond_expr_ty_rec.ty, KolgaTy::Bool));

                // The step expression should be a number
                let step_expr_ty_rec = for_step_expr.get_ty_rec().unwrap();
                ty_eqs.push(TyMatch::new(step_expr_ty_rec.ty, KolgaTy::Num));

                ty_eqs
            }
            Ast::VarAssignExpr {
                meta: _,
                ref ty_rec,
                ident_tkn: _,
                is_imm: _,
                is_global: _,
                ref value,
            } => match **value {
                Ast::FnCallExpr {
                    meta: _,
                    ty_rec: ref fn_ty_rec,
                    ..
                } => {
                    ty_eqs.push(TyMatch::new(ty_rec.ty.clone(), fn_ty_rec.ty.clone()));
                    ty_eqs
                }
                _ => {
                    ty_eqs.extend(self.gen_ty_eq(value));
                    let val_ty_rec = value.get_ty_rec().unwrap();
                    ty_eqs.push(TyMatch::new(ty_rec.ty.clone(), val_ty_rec.ty));
                    ty_eqs
                }
            },
            Ast::FnDeclStmt {
                meta: _,
                ident_tkn: _,
                fn_params: _,
                ret_ty: _,
                ref fn_body,
                sc: _,
            } => {
                ty_eqs.extend(self.gen_ty_eq(fn_body));
                ty_eqs
            }
            // nothing to do with a function call not being assigned, or a
            // declaration with no value
            Ast::RetStmt {
                meta: _,
                ref ret_expr,
            } => {
                match *ret_expr {
                    Some(ref expr) => ty_eqs.extend(self.gen_ty_eq(expr)),
                    None => (),
                };
                ty_eqs
            }
            Ast::FnCallExpr { .. } => ty_eqs,
            Ast::VarDeclExpr { .. } => ty_eqs,
            Ast::ClassDecl { .. } => unimplemented!(),
            Ast::ClassPropAccess { .. } => unimplemented!(),
            Ast::ClassPropSet { .. } => unimplemented!(),
            Ast::ClassFnCallExpr { .. } => unimplemented!(),
            _ => ty_eqs,
        }
    }
}
