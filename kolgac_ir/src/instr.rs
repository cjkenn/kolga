use std::fmt;

#[derive(Debug)]
pub enum IROperator {
    Mv,
    Jmp,
    JmpE,
    JmpNe,
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for IROperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            IROperator::Mv => "mv".to_string(),
            IROperator::Jmp => "jmp".to_string(),
            IROperator::JmpE => "jmpe".to_string(),
            IROperator::JmpNe => "jmpne".to_string(),
            IROperator::Add => "add".to_string(),
            IROperator::Sub => "sub".to_string(),
            IROperator::Mul => "mul".to_string(),
            IROperator::Div => "div".to_string(),
        };

        write!(f, "{}", formatted)
    }
}

#[derive(Debug)]
pub enum IROperand {
    IRNum(f64),
    IRStr(String),
    IRReg(String),
}

impl fmt::Display for IROperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            IROperand::IRNum(n) => format!("{}", n),
            IROperand::IRStr(s) => s.to_string(),
            IROperand::IRReg(r) => r.to_string(),
        };

        write!(f, "{}", formatted)
    }
}

/// Instr is the representation of an instruction in the three-address
/// linear code for kolga's ir. Each instruction expects two operands,
/// an operator, and a result (or resulting location).
#[derive(Debug)]
pub struct Instr {
    /// First operand.
    op1: IROperand,

    /// Second operand. This is optional for some instructions (ie. mv, jmp).
    op2: Option<IROperand>,

    /// Operator.
    opcode: IROperator,

    /// Result destination.
    result: IROperand,

    /// Current label. This encodes the top level label
    /// of the instruction.
    lbl: String,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let dest = format!("{}", self.result);
        let opc = format!("{}", self.opcode);
        let op1 = format!("{}", self.op1);
        let op2 = match &self.op2 {
            Some(o) => format!("{}", o),
            None => "".to_string(),
        };
        let lbl = self.lbl.to_string();

        write!(f, "{} = {} {} {} : {}", dest, opc, op1, op2, lbl)
    }
}

impl Instr {
    pub fn build(
        op1: IROperand,
        op2: Option<IROperand>,
        opc: IROperator,
        result: IROperand,
        lbl: String,
    ) -> Instr {
        Instr {
            op1: op1,
            op2: op2,
            opcode: opc,
            result: result,
            lbl: lbl,
        }
    }
}
