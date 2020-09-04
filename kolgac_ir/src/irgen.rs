use crate::instr::{IROperand, IROperator, Instr};

use kolgac::{
    ast::Ast,
    token::{TknTy, Token},
};

#[derive(Debug)]
pub struct IRGen<'t> {
    /// Syntax tree obtained from parsing. This is assumed to
    /// be valid at this stage.
    pub ast: &'t Ast,

    /// The resulting three address linear code. This is represented
    /// as a vector of instructions.
    pub ir: Vec<Instr>,

    reg: usize,
}

impl<'t> IRGen<'t> {
    pub fn new(ast: &'t Ast) -> IRGen {
        IRGen {
            ast: ast,
            ir: Vec::new(),
            reg: 0,
        }
    }

    pub fn gen(&mut self) {
        match self.ast {
            Ast::Prog { meta: _, stmts } => {
                for stmt in stmts {
                    let iseq = self.instr_seq(stmt);
                    self.ir.extend(iseq);
                }
            }
            _ => panic!("invalid ast provided to ir gen"),
        }
    }

    pub fn instr_seq(&mut self, node: &Ast) -> Vec<Instr> {
        match node {
            Ast::BinaryExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                lhs,
                rhs,
            } => {
                let mut seq = Vec::new();

                let lhs_seq = self.instr_seq(lhs);
                seq.extend(lhs_seq);

                // save current register here, for use in final instr
                let lhs_reg = self.last_used_reg_name();

                let rhs_seq = self.instr_seq(rhs);
                seq.extend(rhs_seq);

                // save current register here again, for second op in last instr
                let rhs_reg = self.last_used_reg_name();

                let op1 = IROperand::IRReg(lhs_reg);
                let op2 = IROperand::IRReg(rhs_reg);
                let op = self.opr_from_tkn(op_tkn);
                let dest = IROperand::IRReg(self.reg_name());

                let bin_instr = Instr::build(op1, Some(op2), op, dest, String::new());
                seq.push(bin_instr);

                seq
            }
            Ast::PrimaryExpr {
                meta: _, ty_rec, ..
            } => {
                let op1 = match &ty_rec.tkn.ty {
                    TknTy::Str(s) => IROperand::IRStr(s.to_string()),
                    TknTy::Val(v) => IROperand::IRNum(*v),
                    TknTy::Ident(i) => IROperand::IRStr(i.to_string()),
                    _ => panic!("invalid primary tkn ty"),
                };

                let opr = IROperator::Mv;
                let dest = IROperand::IRReg(self.reg_name());
                self.next_reg();

                vec![Instr::build(op1, None, opr, dest, String::new())]
            }
            _ => Vec::new(),
        }
    }

    fn reg_name(&self) -> String {
        format!("r{}", self.reg)
    }

    fn next_reg(&mut self) {
        // TOOD: gets more complex with reg constraints.
        self.reg = self.reg + 1
    }

    fn last_used_reg_name(&self) -> String {
        format!("r{}", self.reg - 1)
    }

    fn opr_from_tkn(&self, tkn: &Token) -> IROperator {
        match tkn.ty {
            TknTy::Plus => IROperator::Add,
            TknTy::Minus => IROperator::Sub,
            TknTy::Star => IROperator::Mul,
            TknTy::Slash => IROperator::Div,
            _ => panic!("invalid operator token"),
        }
    }
}
