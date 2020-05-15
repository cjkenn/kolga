use crate::sym::Sym;

use std::{collections::HashMap, rc::Rc};

type Scope = HashMap<String, Rc<Sym>>;

pub struct SymbolTable {
    /// Current scope level in the symbol table. 0 is the global scope,
    /// and when the table is created we allocate a new hashmap to hold that scope.
    /// (ie. manually creating the global scope after creating this struct is NOT required).
    curr_level: usize,

    /// The actual symbol table, as a stack of maps. Each new block scope is pushed onto
    /// this stack, and popped off/finalized when we exit the block.
    table: Vec<Scope>,

    /// Current level of the finalized scope. This is different from the current
    /// level of the symbol table, because we may have created/closed many
    /// scopes before we finalize one (or vice versa).
    pub finalized_level: usize,

    /// The finalized symbol table. We initialize a finalized global scope when
    /// we create the table, because we expect the global scope to be
    /// finalized after parsing.
    finalized: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            curr_level: 0,
            table: vec![HashMap::new()],
            finalized_level: 0,
            finalized: vec![HashMap::new()],
        }
    }

    /// Creates a new scope and pushes it onto the scope stack.
    /// This should be called at the entry of each block in order to properly
    /// block scope statements.
    pub fn init_sc(&mut self) {
        self.curr_level = self.curr_level + 1;
        self.table.push(HashMap::new());
    }

    /// Pops a scope off of the scope stack and finalizes it. The finalize process
    /// takes all the variables in the scope and its parent scopes and stores them
    /// in a new map that we can save for looking up later. After the parsing
    /// phase is over, all scopes as finalized and nothing new can be added.
    /// Returns the index into the finalized table so that we can look it up
    /// after parsing. This index can be stored in AST nodes.
    pub fn finalize_sc(&mut self) -> usize {
        // Store the current state of the table into a scope.
        let mut final_sc: Scope = HashMap::new();
        for scope in &self.table {
            for (key, val) in scope.iter() {
                final_sc.insert(key.to_string(), Rc::clone(val));
            }
        }

        // Remove the current level from the symbol table and add it to
        // our finalized table.
        self.finalized_level = self.finalized_level + 1;
        self.finalized.push(final_sc);
        let final_level = self.finalized_level;

        self.table.pop();
        self.curr_level = self.curr_level - 1;

        final_level
    }

    /// Finalize the global scope after parsing is complete.
    pub fn finalize_global_sc(&mut self) {
        let mut global_sc: Scope = HashMap::new();
        for (key, val) in self.table[0].iter() {
            global_sc.insert(key.to_string(), Rc::clone(val));
        }

        self.finalized[0] = global_sc;
    }

    /// True if the current scope is the global scope, false otherwise.
    pub fn is_global(&self) -> bool {
        self.curr_level == 0
    }

    /// Return the current scope level in the non-finalized table.
    pub fn level(&self) -> usize {
        self.curr_level
    }

    /// Store a symbol in the table at the current level.
    pub fn store(&mut self, key: &str, sym: Sym) {
        self.table[self.curr_level].insert(String::from(key), Rc::new(sym));
    }

    /// Get a symbol from the table. We check the current scope and
    /// all parent scopes for the symbol.
    pub fn retrieve(&self, key: &str) -> Option<Rc<Sym>> {
        let mut curr = self.curr_level;

        while curr >= 0 {
            match self.table[curr].get(key) {
                Some(val) => {
                    return Some(Rc::clone(&val));
                }
                None if curr == 0 => {
                    break;
                }
                None => (),
            };
            curr = curr - 1;
        }

        None
    }

    /// Get a symbol from a finalized scope level. We don't need to search
    /// up any scope stack, since all viable scope levels are put into one
    /// level of the finalized map.
    pub fn retrieve_from_finalized_sc(&self, key: &str, level: usize) -> Option<Rc<Sym>> {
        match self.finalized[level].get(key) {
            Some(val) => Some(Rc::clone(&val)),
            None => None,
        }
    }
}
