use std::rc::Rc;

use error::ty::{TypeErr, TypeErrTy};
use kolgac::ast::Ast;
use kolgac::sym::Sym;
use kolgac::symtab::SymbolTable;
use kolgac::token::{TknTy, Token};
use kolgac::ty_rec::KolgaTy;

pub struct TyCheck<'t, 's> {
    ast: &'t Ast,
    symtab: &'s mut SymbolTable,
    errors: Vec<TypeErr>,
}

impl<'t, 's> TyCheck<'t, 's> {
    pub fn new(ast: &'t Ast, symtab: &'s mut SymbolTable) -> TyCheck<'t, 's> {
        TyCheck {
            ast: ast,
            symtab: symtab,
            errors: Vec::new(),
        }
    }

    /// Initial entry point into the type checker. Loops through each statement in the
    /// AST and type checks them. Returns a vector of errors encountered during type
    /// checking.
    pub fn check(&mut self) -> Vec<TypeErr> {
        match self.ast {
            Ast::Prog { meta: _, stmts } => {
                for stmt in stmts {
                    // Pass in 0 for the global scope.
                    self.check_stmt(stmt.clone(), 0);
                }
            }
            _ => (),
        }

        self.errors.clone()
    }

    /// Type checks a statement. This is a recursive function that continually walks the AST
    /// until a leaf node. Takes in a statement AST and a scope level to look up symbols
    /// in the symbol table. We expect the symbol table to be finalized at this point,
    /// since parsing should be complete. Thus, the final_sc parameter represents the index
    /// into the finalized_scopes vector in the symbol table.
    fn check_stmt(&mut self, stmt: Ast, final_sc: usize) {
        match stmt {
            // ignore var declaration without assign (nothing to check)
            Ast::VarDeclExpr { .. } => (),
            Ast::VarAssignExpr { .. } => {
                self.check_var_assign(&stmt, final_sc);
            }
            Ast::ExprStmt { meta: _, expr } => {
                match *expr.clone() {
                    Ast::FnCallExpr { .. } => {
                        self.check_fn_params(*expr, final_sc);
                        ()
                    }
                    Ast::ClassFnCallExpr {
                        meta: _,
                        class_tkn: _,
                        class_name: _,
                        fn_tkn: _,
                        fn_params: _,
                        sc,
                    } => {
                        self.check_fn_params(*expr, sc);
                        ()
                    }
                    _ => {
                        let astptr = *expr;
                        self.check_expr(&astptr, final_sc);
                        ()
                    }
                };
            }
            Ast::IfStmt {
                meta: _,
                cond_expr,
                if_stmts,
                elif_exprs,
                el_stmts,
            } => {
                let expr = *cond_expr;
                self.check_expr(&expr, final_sc);
                self.check_stmt(*if_stmts, final_sc);

                for stmt in &elif_exprs {
                    self.check_stmt(stmt.clone(), final_sc);
                }

                for stmt in &el_stmts {
                    self.check_stmt(stmt.clone(), final_sc);
                }
            }
            Ast::WhileStmt {
                meta: _,
                cond_expr,
                stmts,
            } => {
                let expr = *cond_expr;
                self.check_expr(&expr, final_sc);
                self.check_stmt(*stmts, final_sc);
            }
            Ast::ElifStmt {
                meta: _,
                cond_expr,
                stmts,
            } => {
                let expr = *cond_expr;
                self.check_expr(&expr, final_sc);
                self.check_stmt(*stmts, final_sc);
            }
            Ast::ForStmt {
                meta: _,
                for_var_decl,
                for_cond_expr,
                for_step_expr,
                stmts,
            } => {
                self.check_stmt(*for_var_decl, final_sc);
                self.check_stmt(*for_cond_expr, final_sc);
                self.check_stmt(*for_step_expr, final_sc);
                self.check_stmt(*stmts, final_sc);
            }
            Ast::BlckStmt { meta: _, stmts, sc } => {
                for stmt in &stmts {
                    self.check_stmt(stmt.clone(), sc);
                }
            }
            Ast::FnDeclStmt {
                meta: _,
                ident_tkn,
                fn_params: _,
                ret_ty,
                fn_body,
                sc,
            } => {
                let fn_ty = ret_ty.ty;
                let fn_stmts = *fn_body;
                match fn_stmts {
                    Ast::BlckStmt {
                        meta: _,
                        stmts,
                        sc: inner_sc,
                    } => {
                        self.check_fn_stmts(&ident_tkn, fn_ty, stmts, inner_sc);
                    }
                    _ => self.check_stmt(fn_stmts, sc),
                };
            }
            Ast::ClassDeclStmt {
                meta: _,
                ty_rec: _,
                ident_tkn: _,
                methods,
                props,
                prop_pos: _,
                sc,
            } => {
                for prop_stmt in props {
                    self.check_stmt(prop_stmt.clone(), sc);
                }

                for stmt in &methods {
                    self.check_stmt(stmt.clone(), sc);
                }
            }
            _ => panic!("{:?} Unrecognized statement type found!", stmt),
        }
    }

    fn check_fn_stmts(
        &mut self,
        fn_tkn: &Token,
        fn_ret_ty: KolgaTy,
        stmts: Vec<Ast>,
        sc_lvl: usize,
    ) {
        let mut has_ret_stmt = false;

        for maybe_stmt in &stmts {
            let stmt = maybe_stmt.clone();
            match stmt {
                Ast::RetStmt { meta: _, ret_expr } => {
                    has_ret_stmt = true;
                    if ret_expr.is_none() {
                        if fn_ret_ty != KolgaTy::Void {
                            self.ty_mismatch(&fn_tkn, &fn_ret_ty, &KolgaTy::Void);
                        }
                    } else {
                        let rhs_ty = self.check_expr(&ret_expr.clone().unwrap(), sc_lvl);
                        if fn_ret_ty != rhs_ty {
                            self.ty_mismatch(&fn_tkn, &fn_ret_ty, &rhs_ty);
                        }
                    }
                }
                _ => self.check_stmt(stmt, sc_lvl),
            };
        }

        if fn_ret_ty != KolgaTy::Void && !has_ret_stmt {
            self.error(
                fn_tkn.line,
                fn_tkn.pos,
                TypeErrTy::InvalidRet(fn_tkn.get_name(), fn_ret_ty.to_string()),
            );
        }
    }

    fn check_expr(&mut self, expr: &Ast, final_sc: usize) -> KolgaTy {
        match expr {
            Ast::VarAssignExpr { .. } => self.check_var_assign(expr, final_sc),
            Ast::UnaryExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                rhs,
            } => {
                if rhs.is_primary() {
                    let rhs_ty_rec = rhs.get_ty_rec().unwrap();
                    return self.reduce_unary_ty(op_tkn.clone(), rhs_ty_rec.ty);
                } else {
                    let rhs_ty = self.check_expr(rhs, final_sc);
                    return self.reduce_unary_ty(op_tkn.clone(), rhs_ty);
                }
            }
            Ast::BinaryExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                lhs,
                rhs,
            }
            | Ast::LogicalExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                lhs,
                rhs,
            } => {
                let lhs_ty_name = self.check_expr(lhs, final_sc);
                let rhs_ty_name = self.check_expr(rhs, final_sc);

                self.reduce_bin_ty(op_tkn.clone(), lhs_ty_name, rhs_ty_name)
            }
            Ast::PrimaryExpr { meta: _, ty_rec }
            | Ast::ClassDeclStmt {
                meta: _, ty_rec, ..
            }
            | Ast::FnCallExpr {
                meta: _, ty_rec, ..
            } => ty_rec.ty.clone(),
            Ast::ClassPropAccess {
                meta: _,
                ident_tkn: _,
                prop_name,
                idx: _,
                owner_class,
            } => self.extract_prop_ty(&owner_class, prop_name.clone()),
            _ => panic!("Unrecognized expression type found!"),
        }
    }

    /// Given a class declaration, find the type of a property in the class. Because
    /// a class does not maintain a mapping of properties (right now), we loop through all
    /// available props until we find the name of the expected prop (the second param).
    fn extract_prop_ty(&self, class_decl_ast: &Ast, prop_name: String) -> KolgaTy {
        match class_decl_ast {
            Ast::ClassDeclStmt {
                meta: _,
                ty_rec: _,
                ident_tkn: _,
                methods: _,
                props,
                ..
            } => {
                let mut prop_ty = None;
                // TODO: this is O(n) and not efficient (should store props in a map)
                for prop in props {
                    match prop.clone() {
                        Ast::VarDeclExpr {
                            ref ty_rec,
                            ref ident_tkn,
                            ..
                        }
                            if ident_tkn.get_name() == prop_name =>
                        {
                            prop_ty = Some(ty_rec.ty.clone());
                        }
                        Ast::VarAssignExpr {
                            ref ty_rec,
                            ref ident_tkn,
                            ..
                        }
                            if ident_tkn.get_name() == prop_name =>
                        {
                            prop_ty = Some(ty_rec.ty.clone());
                        }
                        _ => (),
                    };
                }

                if prop_ty.is_none() {
                    panic!("Cannot type check variable that isnt declared in class");
                }

                return prop_ty.unwrap();
            }
            _ => panic!("Cannot extract property type from this ast"),
        }
    }

    fn check_fn_params(&mut self, fn_call_ast: Ast, final_sc: usize) {
        match fn_call_ast {
            Ast::FnCallExpr {
                meta: _,
                ty_rec: _,
                fn_tkn,
                fn_params,
            } => {
                let fn_sym = self.find_fn_sym(&fn_tkn.clone(), final_sc);
                let fn_param_tys = &fn_sym.unwrap().fn_params.clone().unwrap();

                let mut passed_in_param_tys = Vec::new();

                for ast in &fn_params {
                    passed_in_param_tys.push(self.check_expr(&ast, 0));
                }

                for (idx, mb_ty_rec) in fn_param_tys.iter().enumerate() {
                    let ty_name = mb_ty_rec.clone().ty;
                    if passed_in_param_tys[idx] != ty_name {
                        self.ty_mismatch(&fn_tkn.clone(), &passed_in_param_tys[idx], &ty_name);
                    }
                }
            }
            Ast::ClassFnCallExpr {
                meta: _,
                class_tkn: _,
                class_name: _,
                fn_tkn,
                fn_params,
                sc,
            } => {
                let fn_sym = self.find_fn_sym(&fn_tkn.clone(), sc);
                let fn_param_tys = &fn_sym.unwrap().fn_params.clone().unwrap();

                let mut passed_in_param_tys = Vec::new();

                for ast in &fn_params {
                    passed_in_param_tys.push(self.check_expr(&ast, 0));
                }

                for (idx, mb_ty_rec) in fn_param_tys.iter().enumerate() {
                    let ty_name = mb_ty_rec.clone().ty;
                    if passed_in_param_tys[idx] != ty_name {
                        self.ty_mismatch(&fn_tkn.clone(), &passed_in_param_tys[idx], &ty_name);
                    }
                }
            }
            _ => (),
        }
    }

    /// Reduce a unary ast to the expected type to be returned by the expression.
    fn reduce_unary_ty(&mut self, op_tkn: Token, rhs_ty: KolgaTy) -> KolgaTy {
        match op_tkn.ty {
            TknTy::Minus => {
                if rhs_ty != KolgaTy::Num {
                    self.ty_mismatch(&op_tkn, &KolgaTy::Num, &rhs_ty);
                }
                KolgaTy::Num
            }
            TknTy::Bang => {
                if rhs_ty != KolgaTy::Bool {
                    self.ty_mismatch(&op_tkn, &KolgaTy::Bool, &rhs_ty);
                }
                KolgaTy::Bool
            }
            _ => panic!("Unimplemented unary operator found!"),
        }
    }

    /// Reduce a binary ast so we can check the types in it. Returns the expected type
    /// given the operator, even if there is an error. The expected type is one which we expect
    /// the given operator to evaluate to.
    fn reduce_bin_ty(&mut self, op_tkn: Token, lhs_ty: KolgaTy, rhs_ty: KolgaTy) -> KolgaTy {
        match op_tkn.ty {
            TknTy::Plus | TknTy::Minus | TknTy::Star | TknTy::Slash => {
                // We can only operate on types of the same kind
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    return KolgaTy::Num;
                }

                // Ensure that the types are correct for the given operator
                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                }

                KolgaTy::Num
            }
            TknTy::Gt | TknTy::GtEq | TknTy::Lt | TknTy::LtEq | TknTy::EqEq | TknTy::BangEq => {
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    return KolgaTy::Bool;
                }

                if !lhs_ty.is_numerical() || !rhs_ty.is_numerical() {
                    self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                }

                KolgaTy::Bool
            }
            TknTy::And | TknTy::Or | TknTy::AmpAmp | TknTy::PipePipe => {
                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                    return KolgaTy::Bool;
                }

                if !lhs_ty.is_bool() || !rhs_ty.is_bool() {
                    self.op_mismatch(&op_tkn, &lhs_ty, &rhs_ty);
                }

                KolgaTy::Bool
            }
            _ => panic!("Unimplemented binary operator found!"),
        }
    }

    fn check_var_assign(&mut self, stmt: &Ast, sc: usize) -> KolgaTy {
        match stmt {
            Ast::VarAssignExpr {
                meta: _,
                ty_rec,
                ident_tkn,
                is_imm: _,
                is_global: _,
                value,
            } => {
                let lhs_ty = ty_rec.ty.clone();
                let rhs = value;
                let rhs_ty = self.check_expr(&rhs, sc);

                if lhs_ty != rhs_ty {
                    self.ty_mismatch(&ident_tkn, &lhs_ty, &rhs_ty);
                }

                return lhs_ty;
            }
            _ => panic!("Invalid ast found when checking variable assignment"),
        }
    }

    fn ty_mismatch(&mut self, tkn: &Token, lhs: &KolgaTy, rhs: &KolgaTy) {
        self.error(
            tkn.line,
            tkn.pos,
            TypeErrTy::TyMismatch(lhs.to_string(), rhs.to_string()),
        );
    }

    fn op_mismatch(&mut self, tkn: &Token, lhs: &KolgaTy, rhs: &KolgaTy) {
        let ty = tkn.ty.clone();
        let op_desired = if ty.is_numerical_op() {
            (KolgaTy::Num, KolgaTy::Num)
        } else {
            (KolgaTy::Bool, KolgaTy::Bool)
        };

        let err_ty = TypeErrTy::BinOpMismatch(
            tkn.ty.to_string(),
            op_desired.0.to_string(),
            op_desired.1.to_string(),
            lhs.to_string(),
            rhs.to_string(),
        );

        self.error(tkn.line, tkn.pos, err_ty);
    }

    fn find_fn_sym(&mut self, ident_tkn: &Token, sc: usize) -> Option<Rc<Sym>> {
        let name = ident_tkn.get_name();
        let sym = self.symtab.retrieve_from_finalized_sc(&name, sc);
        match sym {
            Some(symbol) => Some(symbol),
            None => {
                self.error(ident_tkn.line, ident_tkn.pos, TypeErrTy::InvalidFn(name));
                None
            }
        }
    }

    fn error(&mut self, line: usize, pos: usize, ty: TypeErrTy) {
        let err = TypeErr::new(line, pos, ty);
        self.errors.push(err);
    }
}
