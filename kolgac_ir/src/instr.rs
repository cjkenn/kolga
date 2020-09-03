#[derive(Debug)]
pub enum OpCode {
    Mv,
    Jmp,
    JmpE,
    JmpNe,
    Plus,
    Sub,
    Mul,
    Div,
}

/// Instr is the representation of an instruction in the three-address
/// linear code for kolga's ir. Each instruction expects two operands,
/// an operator, and a result (or resulting location).
#[derive(Debug)]
pub struct Instr {
    /// First operand.
    op1: String,

    /// Second operand.
    op2: String,

    /// Operator.
    opcode: OpCode,

    /// Result destination.
    result: String,

    /// Current label. This encodes the top level label
    /// of the instruction.
    lbl: String,
}

impl Instr {
    pub fn build(op1: String, op2: String, opc: OpCode, result: String, lbl: String) -> Instr {
        Instr {
            op1: op1,
            op2: op2,
            opcode: opc,
            result: result,
            lbl: lbl,
        }
    }
}
