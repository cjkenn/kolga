use std::collections::HashMap;

use llvm_sys::prelude::LLVMValueRef;

pub struct ValTab {
    valtab: HashMap<String, LLVMValueRef>
}

impl ValTab {
    pub fn new() -> ValTab {
        ValTab {
            valtab: HashMap::new()
        }
    }

    pub fn store(&mut self, key: &str, val: LLVMValueRef) {
        self.valtab.insert(String::from(key), val);
    }

    pub fn retrieve(&mut self, key: &str) -> Option<LLVMValueRef> {
        match self.valtab.get(key) {
            Some(val) => Some(*val),
            None => None
        }
    }
}
