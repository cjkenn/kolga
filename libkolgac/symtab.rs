use std::collections::HashMap;
use std::rc::Rc;

use sym::Sym;

pub struct SymTab {
    level: usize,
    symtab: Vec<HashMap<String, Rc<Sym>>>
}

impl SymTab {
    pub fn new() -> SymTab {
        SymTab {
            level: 0,
            symtab: vec![HashMap::new()]
        }
    }

    pub fn open_scope(&mut self) {
        self.level = self.level + 1;
        if self.level == self.symtab.len() {
            self.symtab.push(HashMap::new());
        }
    }

    pub fn close_scope(&mut self) {
        self.level = self.level - 1;
    }

    pub fn curr_level(&self) {
        self.level
    }

    pub fn store(&mut self, key: &str, sym: Sym) {
        if self.level <= 0 {
            panic!("No scope initialized!");
        }

        self.symtab[self.level].insert(String::from(key), Rc::new(sym));
    }

    pub fn retrieve(&self, key: &str) -> Option<Rc<Sym>> {
        let mut curr_lvl = self.level;

        while curr_lvl > 0 {
            match self.symtab[curr_lvl].get(key) {
                Some(val) => { return Some(Rc::clone(&val)); }
                None => ()
            };
            curr_lvl = curr_lvl - 1;
        }

        None
    }

    pub fn retrieve_at_level(&self, key: &str, level: usize) -> Option<Rc<Sym>> {
        match self.symtab[level].get(key) {
            Some(val) => Some(Rc::clone(&val)),
            None => None
        }
    }
}
