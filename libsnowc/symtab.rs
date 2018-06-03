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
}
