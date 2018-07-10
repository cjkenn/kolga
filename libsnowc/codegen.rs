use ast::Ast;
use token::{TknTy, Token};
use symtab::SymTab;
use vm::reg::RegPool;
use vm::op::OpCode;

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
                    let mut op_vec = self.gen_stmt(&stmt);
                    ops.append(&mut op_vec);
                }
            },
            _ => ()
        };

        ops
    }

    fn gen_stmt(&mut self, stmt: &Ast) -> Vec<OpCode> {
        match stmt {
            Ast::ExprStmt(maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                self.gen_expr(&ast)
            },
            _ => unimplemented!()
        }
    }

    fn gen_expr(&mut self, expr: &Ast) -> Vec<OpCode> {
        match expr {
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs = maybe_lhs.clone().unwrap();
                let rhs = maybe_rhs.clone().unwrap();
                self.gen_bin_op(op_tkn, &lhs, &rhs)
            },
            _ => unimplemented!()
        }
    }

    fn gen_bin_op(&mut self, op_tkn: &Token, lhs: &Ast, rhs: &Ast) -> Vec<OpCode> {
        match op_tkn.ty {
            TknTy::Plus => {
                if lhs.is_primary() && rhs.is_primary() {
                    let r_reg = self.reg_pool.alloc();
                    let l_reg = self.reg_pool.alloc();
                    let dest = self.reg_pool.alloc();
                    let l_val = lhs.extract_primary_ty_rec().tkn.get_val();
                    let r_val = rhs.extract_primary_ty_rec().tkn.get_val();

                    let l_op = OpCode::MvVal(l_reg.clone(), l_val);
                    let r_op = OpCode::MvVal(r_reg.clone(), r_val);
                    let op = OpCode::Add(dest, l_reg, r_reg);

                    return vec![l_op, r_op, op];
                } else if lhs.is_primary() && !rhs.is_primary() {
                    let l_reg = self.reg_pool.alloc();
                    let mut r_op_vec = self.gen_expr(rhs);
                    let l_val = lhs.extract_primary_ty_rec().tkn.get_val();

                    let l_op = OpCode::MvVal(l_reg.clone(), l_val);
                    let prev_dest_reg = match r_op_vec[2] {
                        OpCode::Add(ref dest,_,_) => dest.clone(),
                        _ => panic!("No destination register found in add!")
                    };

                    let dest = self.reg_pool.alloc();
                    let op = OpCode::Add(dest, l_reg, prev_dest_reg);
                    let mut op_vec = vec![l_op, op];

                    r_op_vec.append(&mut op_vec);
                    return r_op_vec;
                } else if !lhs.is_primary() && rhs.is_primary() {
                    let r_reg = self.reg_pool.alloc();
                    let mut l_op_vec = self.gen_expr(lhs);
                    let r_val = rhs.extract_primary_ty_rec().tkn.get_val();

                    let r_op = OpCode::MvVal(r_reg.clone(), r_val);
                    let prev_dest_reg = match l_op_vec[2] {
                        OpCode::Add(ref dest,_,_) => dest.clone(),
                        _ => panic!("No destination register found in add!")
                    };

                    let dest = self.reg_pool.alloc();
                    let op = OpCode::Add(dest, r_reg, prev_dest_reg);
                    let mut op_vec = vec![r_op, op];

                    l_op_vec.append(&mut op_vec);
                    return l_op_vec;
                } else {
                    unimplemented!()
                }
            },
            TknTy::Minus => {
                unimplemented!()
            },
            TknTy::Star => {
                unimplemented!()
            },
            TknTy::Slash => {
                unimplemented!()
            },
            _ => unimplemented!("Invalid binary operator found in gen")
        }
    }
}
