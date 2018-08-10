use llvm_sys::LLVMRealPredicate;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

use kolgac::ast::Ast;
use kolgac::symtab::SymTab;
use kolgac::token::TknTy;
use kolgac::type_record::{TyRecord, TyName};

use errors::ErrCodeGen;

use valtab::ValTab;

const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;

macro_rules! c_str {
    ($s:expr) => (
        concat!($s, "\0").as_ptr() as *const i8
    );
}

pub struct Gen<'t, 's, 'v> {
    ast: &'t Ast,
    symtab: &'s mut SymTab,
    valtab: &'v mut ValTab,
    errors: Vec<ErrCodeGen>,
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef
}

impl<'t, 's, 'v> Drop for Gen<'t, 's, 'v> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

impl<'t, 's, 'v> Gen<'t, 's, 'v> {
    pub fn new(ast: &'t Ast, symtab: &'s mut SymTab, valtab: &'v mut ValTab) -> Gen<'t, 's, 'v> {
        unsafe {
            let context = LLVMContextCreate();
            Gen {
                ast: ast,
                symtab: symtab,
                valtab: valtab,
                errors: Vec::new(),
                context: context,
                module: LLVMModuleCreateWithNameInContext(c_str!("kolga"), context),
                builder: LLVMCreateBuilderInContext(context)
            }
        }
    }

    pub fn gen(&mut self) {
        match *self.ast {
            Ast::Prog(ref stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt);
                }
            },
            _ => ()
        }

        unsafe {
            LLVMDumpModule(self.module);
        }
    }

    // If empty, nothing to return. Bit of a hack, should probably return a result
    // instead of an empty vec (statements dont evaluate to anything, so there's never an
    // LLVMValueRef returned). But in the case that we do have to generate an expression,
    // we need to know which values we generated.
    fn gen_stmt(&mut self, stmt: &Ast) -> Vec<LLVMValueRef> {
        match stmt {
            Ast::IfStmt(mb_if_cond, mb_then_stmts, mb_else_cond, mb_else_stmts) => {
                unsafe {
                    let cond_val = self.gen_expr(&mb_if_cond.clone().unwrap());
                    if cond_val.is_none() {
                        let msg = format!("Error: codegen failed for ast {:?}", stmt);
                        self.errors.push(ErrCodeGen::new(msg));
                        return Vec::new();
                    }

                    let insert_bb = LLVMGetInsertBlock(self.builder);
                    let mut function = LLVMGetBasicBlockParent(insert_bb);
                    let mut then_bb = LLVMAppendBasicBlockInContext(self.context,
                                                                    function,
                                                                    c_str!("thenblck"));
                    let mut else_bb = LLVMAppendBasicBlockInContext(self.context,
                                                                    function,
                                                                    c_str!("elseblck"));
                    let mut merge_bb = LLVMAppendBasicBlockInContext(self.context,
                                                                     function,
                                                                     c_str!("mergeblck"));

                    LLVMBuildCondBr(self.builder, cond_val.unwrap(), then_bb, else_bb);
                    LLVMPositionBuilderAtEnd(self.builder, then_bb);

                    let then_stmts = mb_then_stmts.clone().unwrap();
                    let mut then_expr_vals = self.gen_stmt(&then_stmts);

                    // Branch from then block to final block
                    LLVMBuildBr(self.builder, merge_bb);
                    let then_end_bb = LLVMGetInsertBlock(self.builder);

                    // Generate else block (if needed). If there is an else condition,
                    // we need to generate code for that as well. If there is an else
                    // block with no condition, generate it.
                    let has_else_br = mb_else_stmts.is_some();
                    let mut else_expr_vals = Vec::new();
                    let mut mb_else_end_bb = None;

                    if has_else_br {
                        LLVMPositionBuilderAtEnd(self.builder, else_bb);
                        let else_stmts = mb_else_stmts.clone().unwrap();
                        else_expr_vals = self.gen_stmt(&else_stmts);

                        // Branch from else block to final block
                        LLVMBuildBr(self.builder, merge_bb);
                        mb_else_end_bb = Some(LLVMGetInsertBlock(self.builder));
                    }

                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let phi_bb = LLVMBuildPhi(self.builder, self.float_ty(), c_str!("phiblck"));
                    LLVMAddIncoming(phi_bb, then_expr_vals.as_mut_ptr(), vec![then_end_bb].as_mut_ptr(), 1);

                    if has_else_br {
                        let else_end_bb = mb_else_end_bb.unwrap();
                        LLVMAddIncoming(phi_bb, else_expr_vals.as_mut_ptr(), vec![else_end_bb].as_mut_ptr(), 1);
                    }

                    Vec::new()
                }
            },
            Ast::BlckStmt(stmts) => {
                let mut generated = Vec::new();
                for stmt in stmts {
                    let mb_gen = self.gen_stmt(&stmt.clone().unwrap());
                    generated.extend(mb_gen);
                }

                generated
            },
            Ast::ExprStmt(maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                // TODO: Don't need to wrap this in an anonymous function. Probably easier to just
                // require at least a top level function from parsing?
                // let anon_fn_ty = LLVMFunctionType(self.void_ty(), ptr::null_mut(), 0, LLVM_FALSE);
                // let anon_fn = LLVMAddFunction(self.module, c_str!("_anon"), anon_fn_ty);
                // let anon_bb = LLVMAppendBasicBlockInContext(self.context, anon_fn, c_str!("_anon"));
                // LLVMPositionBuilderAtEnd(self.builder, anon_bb);

                let val = self.gen_expr(&ast);
                match val {
                    Some(exprval) => vec![exprval],
                    None => {
                        let msg = format!("Error: codegen failed for ast {:?}", ast);
                        self.errors.push(ErrCodeGen::new(msg));

                        Vec::new()
                    }
                }
            },
            Ast::FnDecl(ident_tkn, params, ret_ty_rec, body) => {
                unsafe {
                    let fn_name = self.c_str_from_val(&ident_tkn.get_name());
                    let fn_ty = self.llvm_ty_from_ty_rec(ret_ty_rec);

                    let mut param_tys = self.llvm_tys_from_ty_rec_arr(params);
                    let llvm_fn_ty = LLVMFunctionType(fn_ty,
                                                      param_tys.as_mut_ptr(),
                                                      param_tys.len() as u32,
                                                      LLVM_FALSE);

                    let llvm_fn = LLVMAddFunction(self.module, fn_name, llvm_fn_ty);
                    self.valtab.store(&ident_tkn.get_name(), llvm_fn);

                    let fn_bb = LLVMAppendBasicBlockInContext(self.context, llvm_fn, fn_name);
                    LLVMPositionBuilderAtEnd(self.builder, fn_bb);

                    // TODO: this is hard to read
                    match body.clone().unwrap() {
                        Ast::BlckStmt(stmts) => {
                            for stmt in stmts {
                                match stmt.clone().unwrap() {
                                    Ast::RetStmt(mb_expr) => {
                                        let llvm_val = self.gen_expr(&mb_expr.clone().unwrap());
                                        LLVMBuildRet(self.builder, llvm_val.unwrap());
                                    },
                                    _ => { self.gen_stmt(&stmt.clone().unwrap()); }
                                }
                            }
                        },
                        _ => ()
                    }
                }

                Vec::new()
            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", stmt)
        }
    }

    fn gen_expr(&mut self, expr: &Ast) -> Option<LLVMValueRef> {
        match expr {
            Ast::Primary(prim_ty_rec) => {
                return self.gen_primary(&prim_ty_rec);
            },
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) |
            Ast::Logical(op_tkn, maybe_lhs, maybe_rhs) => {
                let mb_lhs_llvm_val = self.gen_expr(&maybe_lhs.clone().unwrap());
                let mb_rhs_llvm_val = self.gen_expr(&maybe_rhs.clone().unwrap());

                if mb_lhs_llvm_val.is_none() || mb_rhs_llvm_val.is_none() {
                    return None;
                }

                let lhs_llvm_val = mb_lhs_llvm_val.unwrap();
                let rhs_llvm_val = mb_rhs_llvm_val.unwrap();

                self.llvm_val_from_op(&op_tkn.ty, lhs_llvm_val, rhs_llvm_val)
            },
            Ast::FnCall(mb_ident_tkn, params) => {
                let fn_name = mb_ident_tkn.clone().unwrap().get_name();
                let llvm_fn = self.valtab.retrieve(&fn_name);
                if llvm_fn.is_none() {
                    let msg = format!("Undeclared function call: {:?}", fn_name);
                    self.errors.push(ErrCodeGen::new(msg));
                    return None;
                }

                let mut param_tys: Vec<LLVMValueRef> = Vec::new();
                for param in params {
                    let llvm_val = self.gen_expr(param);
                    if llvm_val.is_none() {
                        let msg = format!("Invalid function call param: {:?}", param);
                        self.errors.push(ErrCodeGen::new(msg));
                        return None;
                    }

                    param_tys.push(llvm_val.unwrap());
                }

                unsafe {
                    return Some(LLVMBuildCall(self.builder,
                                              llvm_fn.unwrap(),
                                              param_tys.as_mut_ptr(),
                                              param_tys.len() as u32,
                                              c_str!("")));
                }

            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", expr)
        }
    }

    fn gen_primary(&mut self, ty_rec: &TyRecord) -> Option<LLVMValueRef> {
        match ty_rec.tkn.ty {
            TknTy::Val(ref val) => unsafe { Some(LLVMConstReal(self.float_ty(), *val)) },
            TknTy::Str(ref lit) => unsafe { Some(LLVMBuildGlobalStringPtr(self.builder,
                                                                          self.c_str_from_val(lit),
                                                                          c_str!("")))},
            TknTy::True => unsafe { Some(LLVMConstInt(self.i8_ty(), 1, LLVM_FALSE)) },
            TknTy::False => unsafe { Some(LLVMConstInt(self.i8_ty(), 0, LLVM_FALSE)) },
            TknTy::Ident(ref name) => self.valtab.retrieve(name),
            _ => unimplemented!("Tkn ty {:?} in unimplemented in codegen", ty_rec.tkn.ty)
        }
    }

    fn llvm_ty_from_ty_rec(&self, ty_rec: &TyRecord) -> LLVMTypeRef {
        match ty_rec.ty.clone().unwrap() {
            TyName::String => self.str_ty(),
            TyName::Num => self.float_ty(),
            TyName::Bool => self.i8_ty(),
            // TODO: class types should be represented as structs in llvm probably
            TyName::Class(_) => unimplemented!("Class types not yet implemented for llvm types!")
        }
    }

    fn llvm_tys_from_ty_rec_arr(&self, ty_recs: &Vec<TyRecord>) -> Vec<LLVMTypeRef> {
        let mut llvm_tys = Vec::new();
        for ty_rec in ty_recs {
            llvm_tys.push(self.llvm_ty_from_ty_rec(&ty_rec));
        }

        llvm_tys
    }

    fn llvm_val_from_op(&self, op: &TknTy, lhs: LLVMValueRef, rhs: LLVMValueRef) -> Option<LLVMValueRef> {
        unsafe {
            match op {
                TknTy::Plus => Some(LLVMBuildFAdd(self.builder, lhs, rhs,c_str!("addtmp"))),
                TknTy::Minus => Some(LLVMBuildFSub(self.builder, lhs, rhs, c_str!("subtmp"))),
                TknTy::Star => Some(LLVMBuildFMul(self.builder, lhs, rhs, c_str!("multmp"))),
                TknTy::Slash => Some(LLVMBuildFDiv(self.builder, lhs, rhs, c_str!("divtmp"))),
                TknTy::AmpAmp | TknTy::And => Some(LLVMBuildAnd(self.builder, lhs, rhs, c_str!("andtmp"))),
                TknTy::PipePipe | TknTy::Or => Some(LLVMBuildOr(self.builder, lhs, rhs, c_str!("ortmp"))),
                TknTy::Lt => Some(LLVMBuildFCmp(self.builder,
                                                LLVMRealPredicate::LLVMRealULT,
                                                lhs,
                                                rhs,
                                                c_str!("lttmp"))),
                TknTy::Gt => Some(LLVMBuildFCmp(self.builder,
                                                LLVMRealPredicate::LLVMRealUGT,
                                                lhs,
                                                rhs,
                                                c_str!("gttmp"))),
                TknTy::LtEq => Some(LLVMBuildFCmp(self.builder,
                                                  LLVMRealPredicate::LLVMRealULE,
                                                  lhs,
                                                  rhs,
                                                  c_str!("ltetmp"))),
                TknTy::GtEq => Some(LLVMBuildFCmp(self.builder,
                                                  LLVMRealPredicate::LLVMRealUGE,
                                                  lhs,
                                                  rhs,
                                                  c_str!("gtetmp"))),
                TknTy::EqEq => Some(LLVMBuildFCmp(self.builder,
                                                  LLVMRealPredicate::LLVMRealUEQ,
                                                  lhs,
                                                  rhs,
                                                  c_str!("eqtmp"))),
                TknTy::BangEq => Some(LLVMBuildFCmp(self.builder,
                                                    LLVMRealPredicate::LLVMRealUNE,
                                                    lhs,
                                                    rhs,
                                                    c_str!("neqtmp"))),
                _ => None
            }
        }
    }

    fn str_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.i8_ty(), 0) }
    }

    fn float_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMFloatTypeInContext(self.context) }
    }

    fn i8_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMInt8TypeInContext(self.context) }
    }

    fn c_str_from_val(&self, val: &str) -> *const i8 {
         format!("{}{}", val, "\0").as_ptr() as *const i8
    }
}
