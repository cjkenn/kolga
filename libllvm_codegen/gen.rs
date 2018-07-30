use llvm_sys::LLVMContext;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::bit_writer::*;

use kolgac::ast::Ast;
use kolgac::symtab::SymTab;
use kolgac::token::TknTy;
use kolgac::type_record::{TyRecord, TyName};

use errors::ErrCodeGen;

use std::collections::HashMap;
use std::ptr;

const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;

macro_rules! c_str {
    ($s:expr) => (
        concat!($s, "\0").as_ptr() as *const i8
    );
}

pub struct Gen<'t, 's> {
    ast: &'t Ast,
    symtab: &'s mut SymTab,
    errors: Vec<ErrCodeGen>,
    valuetab: HashMap<String, LLVMValueRef>,
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef
}

impl<'t, 's> Drop for Gen<'t, 's> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

impl<'t, 's> Gen<'t, 's> {
    pub fn new(ast: &'t Ast, symtab: &'s mut SymTab) -> Gen<'t, 's> {
        unsafe {
            let context = LLVMContextCreate();
            Gen {
                ast: ast,
                symtab: symtab,
                valuetab: HashMap::new(),
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
            Ast::ExprStmt(maybe_ast) => {
                unsafe {
                    // Wrap top level expressions in anonymous functions with no params.
                    let anon_fn_ty = LLVMFunctionType(self.void_ty(), ptr::null_mut(), 0, LLVM_FALSE);
                    let anon_fn = LLVMAddFunction(self.module, c_str!("_anon"), anon_fn_ty);
                    let anon_bb = LLVMAppendBasicBlockInContext(self.context, anon_fn, c_str!("_anon"));

                    LLVMPositionBuilderAtEnd(self.builder, anon_bb);

                    let ast = maybe_ast.clone().unwrap();
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

                    // TODO: handle the fn parameters correctly instead of using null_mut
                    let llvm_fn_ty = LLVMFunctionType(fn_ty, ptr::null_mut(), params.len() as u32, LLVM_FALSE);
                    let llvm_fn = LLVMAddFunction(self.module, fn_name, llvm_fn_ty);
                    let fn_bb = LLVMAppendBasicBlockInContext(self.context, llvm_fn, fn_name);
                    LLVMPositionBuilderAtEnd(self.builder, fn_bb);
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
                unsafe {
                    match op_tkn.ty {
                        TknTy::Plus => {
                            return Some(LLVMBuildFAdd(self.builder,
                                                      lhs_llvm_val,
                                                      rhs_llvm_val,
                                                      c_str!("addtmp")));
                        },
                        TknTy::Minus => {
                            return Some(LLVMBuildFSub(self.builder,
                                                      lhs_llvm_val,
                                                      rhs_llvm_val,
                                                      c_str!("subtmp")));
                        },
                        TknTy::Star => {
                            return Some(LLVMBuildFMul(self.builder,
                                                      lhs_llvm_val,
                                                      rhs_llvm_val,
                                                      c_str!("multmp")));
                        },
                        TknTy::Slash => {
                            return Some(LLVMBuildFDiv(self.builder,
                                                      lhs_llvm_val,
                                                      rhs_llvm_val,
                                                      c_str!("divtmp")));
                        },
                        TknTy::AmpAmp | TknTy::And => {
                            return Some(LLVMBuildAnd(self.builder,
                                                     lhs_llvm_val,
                                                     rhs_llvm_val,
                                                     c_str!("andtmp")));
                        },
                        TknTy::PipePipe | TknTy::Or => {
                            return Some(LLVMBuildOr(self.builder,
                                                    lhs_llvm_val,
                                                    rhs_llvm_val,
                                                    c_str!("ortmp")));
                        }
                        _ => panic!("Invalid binary operator found in codegen")
                    }
                }
            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", expr)
        }
    }

    fn gen_primary(&mut self, ty_rec: &TyRecord) -> Option<LLVMValueRef> {
        match ty_rec.tkn.ty {
            TknTy::Val(ref val) => {
                unsafe { return Some(LLVMConstReal(LLVMFloatType(), *val)); }
            },
            TknTy::True => {
                unsafe { return Some(LLVMConstInt(self.i8_ty(), 1, LLVM_FALSE)); }
            },
            TknTy::False => {
                unsafe { return Some(LLVMConstInt(self.i8_ty(), 0, LLVM_FALSE)); }
            },
            TknTy::Ident(ref name) => {
                let val = self.valuetab.get(name);
                if val.is_none() {
                    return None;
                }

                // TODO: there has to be an easier way
                return Some(*val.clone().unwrap());
            },
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

    fn str_ty(&self) -> LLVMTypeRef {
        // String types are actually arrays of chars (i8) in llvm.
        // TODO: How do we allocate for this array when we don't know the size of the return string?
        unsafe { LLVMArrayType(self.i8_ty(), 128 as u32) }
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
