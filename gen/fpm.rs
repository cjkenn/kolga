use llvm_sys::core::*;
use llvm_sys::prelude::{LLVMModuleRef, LLVMPassManagerRef, LLVMValueRef};
use llvm_sys::transforms::scalar::*;

pub struct FPM {
    /// A reference to the actual function pass manager
    fpm: LLVMPassManagerRef,
}

impl Drop for FPM {
    fn drop(&mut self) {
        unsafe { LLVMDisposePassManager(self.fpm) }
    }
}

impl FPM {
    /// Creates a new pass manager for the given module.
    pub fn new(module: LLVMModuleRef) -> FPM {
        unsafe {
            FPM {
                fpm: LLVMCreateFunctionPassManagerForModule(module),
            }
        }
    }

    /// Initializes the pass manager and add all desired function passes.
    pub fn init(&mut self) {
        unsafe {
            //LLVMAddPromoteMemoryToRegisterPass(self.fpm);
            LLVMAddInstructionCombiningPass(self.fpm);
            LLVMAddReassociatePass(self.fpm);
            LLVMInitializeFunctionPassManager(self.fpm);
        }
    }

    /// Runs the pass manager for a given function.
    pub fn run(&mut self, fn_ref: LLVMValueRef) {
        unsafe {
            LLVMRunFunctionPassManager(self.fpm, fn_ref);
        }
    }
}
