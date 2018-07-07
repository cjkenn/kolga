use reg::Reg;

#[derive(Debug, Clone)]
pub enum OpCode {
    // destination, src
    MvReg(Reg, Reg),

    // destination, value
    MvVal(Reg, f64),

    // destination, src
    Ld(Reg, Reg),

    // destination, src
    St(Reg, Reg),

    // Binary Operators
    // Dest, lhs operand, rhs operand
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg),

    Nop
}
