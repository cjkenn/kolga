use op::OpCode;
use errors::ErrRuntime;
use reg::Reg;

pub struct Vm {
    instrs: Vec<OpCode>,
    iptr: usize
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            instrs: Vec::new(),
            iptr: 0
        }
    }

    pub fn from_instrs(instrs: Vec<OpCode>) -> Vm {
        Vm {
            instrs: instrs,
            iptr: 0
        }
    }

    pub fn run(&mut self) {
        // TODO: no clone here
        for mut instr in self.instrs.clone() {
            self.execute(&mut instr);
        }
    }

    pub fn execute(&mut self, instr: &mut OpCode) -> Result<(), ErrRuntime> {
        match instr {
            OpCode::MvReg(ref mut dest, ref src) => {
                if src.val.is_none() {
                    dest.free();
                } else {
                    dest.set(src.val.unwrap());
                }
            },
            OpCode::MvVal(ref mut dest, ref new_val) => {
                dest.set(*new_val);
            },
            OpCode::Add(ref mut dest, ref lhs, ref rhs) => {
                match self.bin_op_err(lhs, rhs) {
                    Some(err) => return Err(err),
                    None => ()
                };

                let result = lhs.val.unwrap() + rhs.val.unwrap();
                dest.set(result);
            },
            OpCode::Sub(ref mut dest, ref lhs, ref rhs) => {
                match self.bin_op_err(lhs, rhs) {
                    Some(err) => return Err(err),
                    None => ()
                };

                let result = lhs.val.unwrap() - rhs.val.unwrap();
                dest.set(result);
            },
            OpCode::Mul(ref mut dest, ref lhs, ref rhs) => {
                match self.bin_op_err(lhs, rhs) {
                    Some(err) => return Err(err),
                    None => ()
                };

                let result = lhs.val.unwrap() * rhs.val.unwrap();
                dest.set(result);
            },
            OpCode::Div(ref mut dest, ref lhs, ref rhs) => {
                match self.bin_op_err(lhs, rhs) {
                    Some(err) => return Err(err),
                    None => ()
                };

                let result = lhs.val.unwrap() / rhs.val.unwrap();
                dest.set(result);
            },
            _ => unimplemented!()
        };

        Ok(())
    }

    fn bin_op_err(&self, lhs_reg: &Reg, rhs_reg: &Reg) -> Option<ErrRuntime> {
        let mut err = None;

        if lhs_reg.val.is_none() {
            let msg = format!("Cannot perform operation: Register {} has no value", lhs_reg.name);
            err = Some(ErrRuntime::new(msg));
        }

        if rhs_reg.val.is_none() {
            let msg = format!("Cannot perform operation: Register {} has no value", rhs_reg.name);
            err = Some(ErrRuntime::new(msg));
        }

        err
    }
}
