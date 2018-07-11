use ast::Ast;
use token::{TknTy, Token};
use symtab::SymTab;
use vm::reg::RegPool;
use vm::op::OpCode;

struct GenResult {
    pub instrs: Vec<OpCode>,
    pub dest_name: String
}

pub struct CodeGen<'c, 's> {
    ast: &'c Ast,
    symtab: &'s SymTab,
    reg_pool: &'s mut RegPool
}

impl <'c, 's> CodeGen<'c, 's> {
    pub fn new(ast: &'c Ast, symtab: &'s SymTab, regpool: &'s mut RegPool) -> CodeGen<'c, 's> {
        CodeGen {
            ast: ast,
            symtab: symtab,
            reg_pool: regpool
        }
    }

    pub fn gen(&mut self) -> Vec<OpCode> {
        let mut ops = Vec::new();

        match *self.ast {
            Ast::Prog(ref stmts) => {
                for stmt in stmts {
                    let mut gen_result = self.gen_stmt(&stmt);
                    ops.append(&mut gen_result.instrs);
                }
            },
            _ => ()
        };

        ops
    }

    fn gen_stmt(&mut self, stmt: &Ast) -> GenResult {
        match stmt {
            Ast::ExprStmt(maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                self.gen_expr(&ast)
            },
            _ => unimplemented!()
        }
    }

    fn gen_expr(&mut self, expr: &Ast) -> GenResult {
        match expr {
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs = maybe_lhs.clone().unwrap();
                let rhs = maybe_rhs.clone().unwrap();
                self.gen_bin_op(op_tkn, &lhs, &rhs)
            },
            _ => unimplemented!()
        }
    }

    fn gen_bin_op(&mut self, op_tkn: &Token, lhs: &Ast, rhs: &Ast) -> GenResult {
        if lhs.is_primary() && rhs.is_primary() {
            let r_reg = self.reg_pool.alloc();
            let l_reg = self.reg_pool.alloc();
            let l_val = lhs.extract_primary_ty_rec().tkn.get_val();
            let r_val = rhs.extract_primary_ty_rec().tkn.get_val();

            let l_op = OpCode::MvVal(l_reg.clone(), l_val);
            let r_op = OpCode::MvVal(r_reg.clone(), r_val);
            let op = self.get_bin_op_ty(op_tkn, &l_reg, &r_reg);

            return GenResult {
                instrs: vec![l_op, r_op, op],
                dest_name: l_reg
            };
        } else if lhs.is_primary() && !rhs.is_primary() {
            let l_reg = self.reg_pool.alloc();
            let l_val = lhs.extract_primary_ty_rec().tkn.get_val();
            let l_op = OpCode::MvVal(l_reg.clone(), l_val);

            let rhs_gen_result = self.gen_expr(rhs);
            let prev_dest_reg = rhs_gen_result.dest_name;
            let op = self.get_bin_op_ty(op_tkn, &l_reg, &prev_dest_reg);
            let mut rhs_instrs = rhs_gen_result.instrs;

            let mut instrs = vec![l_op, op];

            rhs_instrs.append(&mut instrs);
            return GenResult {
                instrs: rhs_instrs,
                dest_name: prev_dest_reg
            };
        } else if !lhs.is_primary() && rhs.is_primary() {
            let rhs_reg = self.reg_pool.alloc();
            let rhs_val = rhs.extract_primary_ty_rec().tkn.get_val();
            let rhs_op = OpCode::MvVal(rhs_reg.clone(), rhs_val);

            let lhs_gen_result = self.gen_expr(lhs);
            let prev_dest_reg = lhs_gen_result.dest_name;
            let op = self.get_bin_op_ty(op_tkn, &prev_dest_reg, &rhs_reg);
            let mut lhs_instrs = lhs_gen_result.instrs;

            let mut instrs = vec![rhs_op, op];

            lhs_instrs.append(&mut instrs);
            return GenResult {
                instrs: lhs_instrs,
                dest_name: prev_dest_reg
            };
        } else {
            let lhs_gen_result = self.gen_expr(lhs);
            let mut rhs_gen_result = self.gen_expr(rhs);
            let op = self.get_bin_op_ty(op_tkn, &lhs_gen_result.dest_name, &rhs_gen_result.dest_name);

            let mut lhs_instrs = lhs_gen_result.instrs;
            lhs_instrs.append(&mut rhs_gen_result.instrs);
            let mut instrs = vec![op];
            lhs_instrs.append(&mut instrs);

            return GenResult {
                instrs: lhs_instrs,
                dest_name: lhs_gen_result.dest_name
            };
        }
    }

    fn get_bin_op_ty(&self, op_tkn: &Token, lhs_operand: &str, rhs_operand: &str) -> OpCode {
        match op_tkn.ty {
            TknTy::Plus => {
                OpCode::Add(lhs_operand.to_string(),
                            lhs_operand.to_string(),
                            rhs_operand.to_string())
            },
            TknTy::Star => {
                OpCode::Mul(lhs_operand.to_string(),
                            lhs_operand.to_string(),
                            rhs_operand.to_string())
            },
            TknTy::Slash => {
                OpCode::Div(lhs_operand.to_string(),
                            lhs_operand.to_string(),
                            rhs_operand.to_string())
            },
            TknTy::Minus => {
                OpCode::Sub(lhs_operand.to_string(),
                            lhs_operand.to_string(),
                            rhs_operand.to_string())
            },
            _ => panic!("Unknown binary operator found")
        }
    }
}
