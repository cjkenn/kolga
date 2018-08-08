use llvm_sys::LLVMRealPredicate;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

use kolgac::ast::Ast;
use kolgac::symtab::SymTab;
use kolgac::token::TknTy;
use kolgac::type_record::{TyRecord, TyName};

use errors::ErrCodeGen;

use valtab::ValTab;

use std::ptr;

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

    fn gen_stmt(&mut self, stmt: &Ast) {
        match stmt {
            Ast::IfStmt(mb_cond, mb_ifs, mb_else_expr, mb_elses) => {
                unsafe {
                    let cond_val = self.gen_expr(&mb_cond.clone().unwrap());
                    if cond_val.is_none() {
                        let msg = format!("Error: codegen failed for ast {:?}", stmt);
                        self.errors.push(ErrCodeGen::new(msg));
                        return;
                    }

                    let insert_bb = LLVMGetInsertBlock(self.builder);
                    // This can segfault if we don't have a function block we're building. For example,
                    // if 5 < 10 { ... } will segfault, but
                    // func main() num {
                    //    if 5 < 10 { ... }
                    // }
                    // is ok
                    // insert_bb is null without a parent function.
                    // TODO: Can we detect this so we can add an anon function if needed?
                    let mut function = LLVMGetBasicBlockParent(insert_bb);
                    let mut then_bb = LLVMAppendBasicBlockInContext(self.context, function, c_str!("thenblck"));
                    let mut else_bb = LLVMAppendBasicBlockInContext(self.context, function, c_str!("elseblck"));

                    LLVMBuildCondBr(self.builder, cond_val.unwrap(), then_bb, else_bb);

                    LLVMPositionBuilderAtEnd(self.builder, then_bb);
                    // TODO: this is Ast::BlckStmt. Need to get LLVMValueRef from then statements
                    // somehow
                    let ifs_val = self.gen_expr(&mb_ifs.clone().unwrap());
                    if ifs_val.is_none() {
                        let msg = format!("Error: codegen failed for ast {:?}", stmt);
                        self.errors.push(ErrCodeGen::new(msg));
                        return;
                    }
                }
            },
            Ast::BlckStmt(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(&stmt.clone().unwrap());
                }
            },
            Ast::ExprStmt(maybe_ast) => {
                unsafe {
                    let ast = maybe_ast.clone().unwrap();
                    let anon_fn_ty = LLVMFunctionType(self.void_ty(), ptr::null_mut(), 0, LLVM_FALSE);
                    let anon_fn = LLVMAddFunction(self.module, c_str!("_anon"), anon_fn_ty);
                    let anon_bb = LLVMAppendBasicBlockInContext(self.context, anon_fn, c_str!("_anon"));
                    LLVMPositionBuilderAtEnd(self.builder, anon_bb);

                    let val = self.gen_expr(&ast);
                    match val {
                        Some(exprval) => { LLVMBuildRet(self.builder, exprval); },
                        None => {
                            let msg = format!("Error: codegen failed for ast {:?}", ast);
                            self.errors.push(ErrCodeGen::new(msg));
                        }
                    };
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

    fn void_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMVoidTypeInContext(self.context) }
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
