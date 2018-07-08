use reg::Reg;

#[derive(Debug, Clone)]
pub enum OpCode {
    // Move Operators
    // destination, src
    MvReg(String, String),
    MvVal(String, f64),

    // Binary Operators
    // Dest, lhs operand, rhs operand
    Add(String, String, String),
    Sub(String, String, String),
    Mul(String, String, String),
    Div(String, String, String)
}
