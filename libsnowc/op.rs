use reg::Reg;

pub enum OpCode {
    Mv(Reg, Reg),
    St(String, Reg),
    Ld(Reg, String),
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg),
    Neg(Reg, Reg),
    Call(Reg, String),
    Eq(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    Jmp(usize),
    JmpEq(usize),
    JmpNEq(usize),
    JmpGt(usize),
    JmpGte(usize),
    JmpLt(usize),
    JmpLte(usize)
}
