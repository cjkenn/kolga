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
            is_free: false,
            val: None
        }
    }

    pub fn with_val(name: String, val: f64) -> Reg {
        Reg {
            name: name,
            is_free: true,
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
