use std::collections::HashMap;
use llvm_sys::prelude::LLVMValueRef;

type Scope = HashMap<String, LLVMValueRef>;

// TODO: this is very close to the symbol table. Maybe we have an Option<LLVMValueRef> in the
// Sym class? We set it to None in the parser, but update it in the correct scope during
// codegen, then we use the same symbol table throughout.
pub struct ValTab {
    curr_level: usize,
    table: Vec<Scope>
}

impl ValTab {
    pub fn new() -> ValTab {
        ValTab {
            curr_level: 0,
            table: vec![HashMap::new()]
        }
    }

    pub fn init_sc(&mut self) {
        self.curr_level = self.curr_level + 1;
        self.table.push(HashMap::new());
    }

    pub fn close_sc(&mut self) {
        self.table.pop();
        self.curr_level = self.curr_level - 1;
    }

    pub fn store(&mut self, key: &str, val: LLVMValueRef) {
        self.table[self.curr_level].insert(String::from(key), val);
    }

    pub fn retrieve(&mut self, key: &str) -> Option<LLVMValueRef> {
        let mut curr = self.curr_level;

        while curr >= 0 {
            match self.table[curr].get(key) {
                Some(val) => { return Some(*val); },
                None if curr == 0 => { break; },
                None => ()
            };
            curr = curr - 1;
        }

        None
    }
}
