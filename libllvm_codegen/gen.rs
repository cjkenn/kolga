use llvm_sys::LLVMContext;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::bit_writer::*;

use kolgac::ast::Ast;
use kolgac::symtab::SymTab;
use kolgac::token::TknTy;
use kolgac::type_record::TyRecord;

use std::collections::HashMap;

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
                    let val = self.gen_stmt(stmt);
                    unsafe {
                        LLVMDumpValue(val);
                    }
                }
            },
            _ => ()
        }
    }

    fn gen_stmt(&mut self, stmt: &Ast) -> LLVMValueRef {
        match stmt {
            Ast::ExprStmt(maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                return self.gen_expr(&ast);
            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", stmt)
        }
    }

    fn gen_expr(&mut self, expr: &Ast) -> LLVMValueRef {
        match expr {
            Ast::Primary(prim_ty_rec) => {
                return self.gen_primary(&prim_ty_rec);
            },
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs_llvm_val = self.gen_expr(&maybe_lhs.clone().unwrap());
                let rhs_llvm_val = self.gen_expr(&maybe_rhs.clone().unwrap());

                // TODO: check for None values here, when actual error handling is added
                match op_tkn.ty {
                    TknTy::Plus => {
                        unsafe {
                            return LLVMBuildFAdd(self.builder, lhs_llvm_val, rhs_llvm_val, c_str!("addtmp"));
                        }
                    },
                    TknTy::Minus => {
                        unsafe {
                            return LLVMBuildFSub(self.builder, lhs_llvm_val, rhs_llvm_val, c_str!("subtmp"));
                        }
                    },
                    TknTy::Star => {
                        unsafe {
                            return LLVMBuildFMul(self.builder, lhs_llvm_val, rhs_llvm_val, c_str!("multmp"));
                        }
                    },
                    TknTy::Slash => {
                        unsafe {
                            return LLVMBuildFDiv(self.builder, lhs_llvm_val, rhs_llvm_val, c_str!("divtmp"));
                        }
                    },
                    _ => panic!("Invalid binary operator found in codegen")
                }
            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", expr)
        }
    }

    fn gen_primary(&mut self, ty_rec: &TyRecord) -> LLVMValueRef {
        match ty_rec.tkn.ty {
            TknTy::Val(ref val) => {
                unsafe {
                    return LLVMConstReal(LLVMFloatType(), *val);
                }
            },
            TknTy::Ident(ref name) => {
                let val = self.valuetab.get(name);
                if val.is_none() {
                    // TODO: return option from this method and handle errors
                    panic!("Unknown variable found!")
                }

                return val.unwrap().clone();
            },
            _ => unimplemented!("Tkn ty {:?} in unimplemented in codegen", ty_rec.tkn.ty)
        }
    }
}
