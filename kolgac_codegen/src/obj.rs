use llvm_sys::{prelude::*, target::*, target_machine::*};

use std::{
    ffi::{CStr, CString},
    ptr,
};

pub struct ObjGenerator {
    ir: LLVMModuleRef,
}

impl ObjGenerator {
    pub fn new(module: LLVMModuleRef) -> ObjGenerator {
        ObjGenerator { ir: module }
    }

    pub fn emit(&mut self, filename: &str) {
        unsafe {
            let triple = LLVMGetDefaultTargetTriple();

            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            let mut target = ptr::null_mut();
            let mut err_msg = ptr::null_mut();
            LLVMGetTargetFromTriple(triple, &mut target, &mut err_msg);
            if target.is_null() {
                let cmsg = CStr::from_ptr(err_msg as *const _);
                panic!("{:?}", cmsg);
            }

            let cpu = c_str!("generic");
            let features = c_str!("");
            let target_machine = LLVMCreateTargetMachine(
                target,
                triple,
                cpu,
                features,
                LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelDefault,
            );

            let mut gen_obj_error = c_str!("error generating object file") as *mut i8;
            let c_file = CString::new(filename).unwrap();

            let result = LLVMTargetMachineEmitToFile(
                target_machine,
                self.ir,
                c_file.as_ptr() as *mut i8,
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut gen_obj_error,
            );

            if result != 0 {
                let cmsg = CStr::from_ptr(gen_obj_error as *const _);
                panic!("{:?}", cmsg);
            }
        }
    }
}
