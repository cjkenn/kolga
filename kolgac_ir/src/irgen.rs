use crate::instr::{IRArg, IROp, Instr};

use kolgac::{
    ast::Ast,
    token::{TknTy, Token},
    ty_rec::TyRecord,
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
            Ast::ExprStmt { meta: _, expr } => self.instr_seq(expr),
            Ast::BlckStmt { meta: _, stmts, .. } => {
                let mut seq = Vec::new();
                for stmt in stmts {
                    let vr = self.instr_seq(stmt);
                    seq.extend(vr);
                }
                seq
            }
            Ast::VarDeclExpr { .. } => {
                // we don't generate any instructions for an empty declaration,
                // we assume that the ident exists later when we refer to it
                // in expressions.
                Vec::new()
            }
            Ast::VarAssignExpr {
                meta: _,
                ty_rec: _,
                ident_tkn,
                is_imm: _,
                is_global: _,
                value,
            } => {
                let mut seq = Vec::new();
                let assign_seq = self.instr_seq(value);
                seq.extend(assign_seq);

                // We must advance to the next reg first here, in order to
                // correctly get the previously used reg name, which is where the
                // rhs of the assign expr is stored/
                self.next_reg();
                let rhs_reg = self.last_used_reg_name();

                let op1 = IRArg::Str(ident_tkn.get_name());
                let op2 = IRArg::Reg(rhs_reg);
                let dest = IRArg::Reg(self.reg_name());
                let op = IROp::St;

                let bin_instr = Instr::build(op1, Some(op2), op, dest, String::new());
                seq.push(bin_instr);

                seq
            }
            Ast::BinaryExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                lhs,
                rhs,
            }
            | Ast::LogicalExpr {
                meta: _,
                ty_rec: _,
                op_tkn,
                lhs,
                rhs,
            } => self.bin_op(op_tkn, lhs, rhs),
            Ast::PrimaryExpr {
                meta: _, ty_rec, ..
            } => self.primary(ty_rec),

            _ => Vec::new(),
        }
    }

    fn bin_op(&mut self, op_tkn: &Token, lhs: &Ast, rhs: &Ast) -> Vec<Instr> {
        let mut seq = Vec::new();

        let lhs_seq = self.instr_seq(lhs);
        seq.extend(lhs_seq);

        // save current register here, for use in final instr
        let lhs_reg = self.last_used_reg_name();

        let rhs_seq = self.instr_seq(rhs);
        seq.extend(rhs_seq);

        // save current register here again, for second op in last instr
        let rhs_reg = self.last_used_reg_name();

        let op1 = IRArg::Reg(lhs_reg);
        let op2 = IRArg::Reg(rhs_reg);
        let op = self.opr_from_tkn(op_tkn);
        let dest = IRArg::Reg(self.reg_name());

        let bin_instr = Instr::build(op1, Some(op2), op, dest, String::new());
        seq.push(bin_instr);

        seq
    }

    fn primary(&mut self, ty_rec: &TyRecord) -> Vec<Instr> {
        let op1 = match &ty_rec.tkn.ty {
            TknTy::Val(v) => IRArg::Num(*v),
            TknTy::Str(s) => IRArg::Str(s.to_string()),
            TknTy::Ident(i) => IRArg::Str(i.to_string()),
            _ => panic!("invalid primary tkn ty"),
        };

        let opr = match &ty_rec.tkn.ty {
            TknTy::Ident(_) => IROp::Ld,
            _ => IROp::Mv,
        };

        let dest = IRArg::Reg(self.reg_name());
        self.next_reg();

        vec![Instr::build(op1, None, opr, dest, String::new())]
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

    fn opr_from_tkn(&self, tkn: &Token) -> IROp {
        match tkn.ty {
            TknTy::Plus => IROp::Add,
            TknTy::Minus => IROp::Sub,
            TknTy::Star => IROp::Mul,
            TknTy::Slash => IROp::Div,
            TknTy::Lt => IROp::Lt,
            TknTy::Gt => IROp::Gt,
            TknTy::EqEq => IROp::EqEq,
            TknTy::LtEq => IROp::LtEq,
            TknTy::GtEq => IROp::GtEq,
            TknTy::BangEq => IROp::Neq,
            TknTy::AmpAmp => IROp::LogAnd,
            TknTy::PipePipe => IROp::LogOr,
            _ => panic!("invalid operator token"),
        }
    }
}
