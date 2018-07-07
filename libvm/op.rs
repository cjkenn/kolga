use reg::Reg;

#[derive(Debug, Clone)]
pub enum OpCode {
    // Move Operators
    // destination, src
    MvReg(Reg, Reg),
    MvVal(Reg, f64),

    // Binary Operators
    // Dest, lhs operand, rhs operand
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg)
}
