use reg::Reg;

#[derive(Debug, Clone)]
pub enum OpCode {
    // Move Operators: (Destination, Source)
    MvReg(String, String),
    MvVal(String, f64),
    // For ld/st, the destination is a var name
    St(String, String),
    Ld(String, String),

    // Unary Operators: (Destination, Source)
    Neg(String, String),
    Not(String, String),

    // Binary Operators: (Destination, LHS operand, RHS operand)
    Add(String, String, String),
    Sub(String, String, String),
    Mul(String, String, String),
    Div(String, String, String)
}
