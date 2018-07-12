use reg::Reg;

#[derive(Debug, Clone)]
pub enum OpCode {
    // Move Operators
    // destination, src
    MvReg(String, String),
    MvVal(String, f64),
    Ld(String, String),

    // For storage, the destination is a var name
    St(String, String),

    // Binary Operators
    // Dest, lhs operand, rhs operand
    Add(String, String, String),
    Sub(String, String, String),
    Mul(String, String, String),
    Div(String, String, String)
}
