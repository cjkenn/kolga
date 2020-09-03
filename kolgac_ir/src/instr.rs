#[derive(Debug)]
pub enum IROperator {
    Mv,
    Jmp,
    JmpE,
    JmpNe,
    Plus,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum IROperand {
    KolNum(f64),
    KolStr(String),
    KolReg(String),
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
