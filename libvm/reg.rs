use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct RegPool {
    pool: HashMap<String, Reg>,
    cnt: usize
}

impl RegPool {
    pub fn new() -> RegPool {
        RegPool {
            pool: HashMap::new(),
            cnt: 0
        }
    }

    pub fn alloc(&mut self) -> String {
        let name = self.next();
        let reg = Reg::new(name.clone());
        self.pool.insert(name.clone(), reg);
        name
    }

    pub fn get(&self, key: &str) -> Option<&Reg> {
        self.pool.get(key)
    }

    pub fn set(&mut self, key: &str, val: Reg) {
        self.pool.insert(key.to_string(), val);
    }

    pub fn alter(&mut self, key: &str, change: f64) {
        self.pool.get_mut(key).unwrap().set(change);
    }

    // TODO: Infinite registers for now. Could write an
    // allocator and limit registers.
    pub fn next(&mut self) -> String {
        let name = format!("r{}", self.cnt);
        self.cnt = self.cnt + 1;
        name
    }
}

#[derive(Debug, Clone)]
pub struct Reg {
    pub name: String,
    pub is_free: bool,
    pub val: Option<f64>
}

impl Reg {
    pub fn new(name: String) -> Reg {
        Reg {
            name: name,
            is_free: true,
            val: None
        }
    }

    pub fn with_val(name: String, val: f64) -> Reg {
        Reg {
            name: name,
            is_free: false,
            val: Some(val)
        }
    }

    pub fn set(&mut self, new_val: f64) {
        self.is_free = false;
        self.val = Some(new_val);
    }

    pub fn free(&mut self) {
        self.is_free = true;
        self.val = None;
    }
}
