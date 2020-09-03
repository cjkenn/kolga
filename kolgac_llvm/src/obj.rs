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
            let mut target_err_str = ptr::null_mut();
            LLVMGetTargetFromTriple(triple, &mut target, &mut target_err_str);
            if target.is_null() {
                let cmsg = CStr::from_ptr(target_err_str as *const _);
                panic!("{:?}", cmsg);
            }

            let cpu = CString::new("generic").expect("invalid cpu name provided");
            let features = CString::new("").expect("invalid feature provided");

            let target_machine = LLVMCreateTargetMachine(
                target,
                triple,
                cpu.as_ptr() as *const _,
                features.as_ptr() as *const _,
                LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelDefault,
            );

            let output_file = CString::new(filename).expect("invalid filename provided");
            let mut err_str = CString::new("writing obj file failed").unwrap().as_ptr() as *mut _;

            println!("here");

            llvm_sys::core::LLVMDumpModule(self.ir);

            let result = LLVMTargetMachineEmitToFile(
                target_machine,
                self.ir,
                output_file.as_ptr() as *mut i8,
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut err_str,
            );

            println!("here2");

            if result != 0 {
                let cmsg = CStr::from_ptr(err_str as *const _);
                panic!("{:?}", cmsg);
            }

            LLVMDisposeTargetMachine(target_machine);
        }
    }
}
