use std::collections::HashMap;
use std::rc::Rc;

use sym::Sym;

type Scope = HashMap<String, Rc<Sym>>;

pub struct SymbolTable {
    curr_level: usize,
    table: Vec<Scope>,
    finalized_level: usize,
    finalized: Vec<Scope>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            curr_level: 0, // 0 is global scope, already initiliazed below
            table: vec![HashMap::new()],
            finalized_level: 0, // 0 is reserved for the global finalized scope
            finalized: vec![HashMap::new()]
        }
    }

    pub fn init_sc(&mut self) {
        self.curr_level = self.curr_level + 1;
        self.table.push(HashMap::new());
    }

    pub fn finalize_sc(&mut self) -> usize {
        // Store the current state of the table into a scope. We can
        // reference this scope at other times after parsing, such
        // as during type checking.
        let mut final_sc: Scope = HashMap::new();
        for scope in &self.table {
            for (key, val) in scope.iter() {
                final_sc.insert(key.to_string(), Rc::clone(val));
            }
        }

        self.finalized_level = self.finalized_level + 1;
        self.finalized.push(final_sc);
        let final_level = self.finalized_level;

        self.table.pop();
        self.curr_level = self.curr_level - 1;

        final_level
    }

    pub fn finalize_global_sc(&mut self) {
        let mut global_sc: Scope = HashMap::new();
        for (key, val) in self.table[0].iter() {
            global_sc.insert(key.to_string(), Rc::clone(val));
        }

        self.finalized[0] = global_sc;
    }

    pub fn is_global(&self) -> bool {
        self.curr_level == 0
    }

    pub fn level(&self) -> usize {
        self.curr_level
    }

    pub fn store(&mut self, key: &str, sym: Sym) {
        self.table[self.curr_level].insert(String::from(key), Rc::new(sym));
    }

    pub fn retrieve(&self, key: &str) -> Option<Rc<Sym>> {
        let mut curr = self.curr_level;

        while curr >= 0 {
            match self.table[curr].get(key) {
                Some(val) => { return Some(Rc::clone(&val)); },
                None if curr == 0 => { break; },
                None => ()
            };
            curr = curr - 1;
        }

        None
    }

    pub fn retrieve_from_finalized_sc(&self, key: &str, level: usize) -> Option<Rc<Sym>> {
        match self.finalized[level].get(key) {
            Some(val) => Some(Rc::clone(&val)),
            None => None
        }
    }
}
