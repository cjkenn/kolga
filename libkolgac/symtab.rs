use std::collections::HashMap;
use std::rc::Rc;

use sym::Sym;

pub struct SymTab {
    symtab: HashMap<String, Rc<Sym>>
}

impl SymTab {
    pub fn new() -> SymTab {
        SymTab {
            symtab: HashMap::new()
        }
    }

    pub fn store(&mut self, key: &str, sym: Sym) {
        self.symtab.insert(String::from(key), Rc::new(sym));
    }

    pub fn retrieve(&self, key: &str) -> Option<Rc<Sym>> {
        match self.symtab.get(key) {
            Some(val) => Some(Rc::clone(&val)),
            None => None
        }
    }
}
