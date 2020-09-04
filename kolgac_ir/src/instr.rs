use std::fmt;

#[derive(Debug)]
pub enum IROp {
    Mv,
    St,
    Ld,

    Jmp,
    JmpE,
    JmpNe,

    Add,
    Sub,
    Mul,
    Div,

    Lt,
    Gt,
    EqEq,
    LtEq,
    GtEq,
    Neq,
    LogAnd,
    LogOr,
}

impl fmt::Display for IROp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            IROp::Mv => "mv".to_string(),
            IROp::St => "st".to_string(),
            IROp::Ld => "ld".to_string(),
            IROp::Jmp => "jmp".to_string(),
            IROp::JmpE => "jmpe".to_string(),
            IROp::JmpNe => "jmpne".to_string(),
            IROp::Add => "add".to_string(),
            IROp::Sub => "sub".to_string(),
            IROp::Mul => "mul".to_string(),
            IROp::Div => "div".to_string(),
            IROp::Lt => "lt".to_string(),
            IROp::Gt => "gt".to_string(),
            IROp::EqEq => "eq".to_string(),
            IROp::LtEq => "lteq".to_string(),
            IROp::GtEq => "gteq".to_string(),
            IROp::Neq => "neq".to_string(),
            IROp::LogAnd => "and".to_string(),
            IROp::LogOr => "or".to_string(),
        };

        write!(f, "{}", formatted)
    }
}

#[derive(Debug)]
pub enum IRArg {
    Num(f64),
    Str(String),
    Reg(String),
}

impl fmt::Display for IRArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            IRArg::Num(n) => format!("{}", n),
            IRArg::Str(s) => s.to_string(),
            IRArg::Reg(r) => r.to_string(),
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
    op1: IRArg,

    /// Second operand. This is optional for some instructions (ie. mv, jmp).
    op2: Option<IRArg>,

    /// Operator.
    opcode: IROp,

    /// Result destination.
    result: IRArg,

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
    pub fn build(op1: IRArg, op2: Option<IRArg>, opc: IROp, result: IRArg, lbl: String) -> Instr {
        Instr {
            op1: op1,
            op2: op2,
            opcode: opc,
            result: result,
            lbl: lbl,
        }
    }
}
