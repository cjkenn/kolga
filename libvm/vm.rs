use op::OpCode;
use errors::ErrRuntime;
use reg::RegPool;

pub struct Vm<'rp> {
    reg_pool: &'rp mut RegPool,
    iptr: usize
}

impl<'rp> Vm<'rp> {
    pub fn new(rpool: &'rp mut RegPool) -> Vm<'rp> {
        Vm {
            reg_pool: rpool,
            iptr: 0
        }
    }

    pub fn run(&mut self, instrs: Vec<OpCode>) {
        for instr in instrs {
            self.execute(&instr);
        }
    }

    pub fn execute(&mut self, instr: &OpCode) -> Result<(), ErrRuntime> {
        match *instr {
            OpCode::MvReg(ref dest_name, ref src_name) => {
                let src = self.reg_pool.get(src_name).unwrap().clone();
                let new_val = src.val.unwrap();
                self.reg_pool.alter(dest_name, new_val);
            },
            OpCode::MvVal(ref dest_name, ref new_val) => {
                self.reg_pool.alter(dest_name, *new_val);
            },
            OpCode::Add(ref dest_name, ref lhs_name, ref rhs_name) => {
                let lhs = self.reg_pool.get(lhs_name).unwrap().clone();
                let rhs = self.reg_pool.get(rhs_name).unwrap().clone();
                let result = lhs.val.unwrap() + rhs.val.unwrap();
                self.reg_pool.alter(dest_name, result);
            },
            OpCode::Sub(ref dest_name, ref lhs_name, ref rhs_name) => {
                let lhs = self.reg_pool.get(lhs_name).unwrap().clone();
                let rhs = self.reg_pool.get(rhs_name).unwrap().clone();
                let result = lhs.val.unwrap() - rhs.val.unwrap();
                self.reg_pool.alter(dest_name, result);
            },
            OpCode::Mul(ref dest_name, ref lhs_name, ref rhs_name) => {
                let lhs = self.reg_pool.get(lhs_name).unwrap().clone();
                let rhs = self.reg_pool.get(rhs_name).unwrap().clone();
                let result = lhs.val.unwrap() * rhs.val.unwrap();
                self.reg_pool.alter(dest_name, result);
            },
            OpCode::Div(ref dest_name, ref lhs_name, ref rhs_name) => {
                let lhs = self.reg_pool.get(lhs_name).unwrap().clone();
                let rhs = self.reg_pool.get(rhs_name).unwrap().clone();
                let result = lhs.val.unwrap() / rhs.val.unwrap();
                self.reg_pool.alter(dest_name, result);
            },
            _ => unimplemented!()
        };

        Ok(())
    }
}
