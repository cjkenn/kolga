use std::collections::HashMap;
use llvm_sys::prelude::LLVMTypeRef;

pub struct ClassTab {
    table: HashMap<String, LLVMTypeRef>
}

impl ClassTab {
    pub fn new() -> ClassTab {
        ClassTab {
            table: HashMap::new()
        }
    }

    pub fn store(&mut self, key: &str, val: LLVMTypeRef) {
        self.table.insert(String::from(key), val);
    }

    pub fn retrieve(&self, key: &str) -> Option<LLVMTypeRef> {
        match self.table.get(key) {
            Some(val) => Some(*val),
            None => None
        }
    }
}
