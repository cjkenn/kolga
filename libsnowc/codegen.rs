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
            Ast::BlckStmt(stmts) => {
                let mut stmt_instrs = Vec::new();
                let mut final_dest = String::new();
                for stmt in stmts {
                    let mut result = self.gen_stmt(&stmt.clone().unwrap());
                    stmt_instrs.append(&mut result.instrs);
                    final_dest = result.dest_name;
                }

                GenResult {
                    instrs: stmt_instrs,
                    dest_name: final_dest.to_string()
                }
            },
            Ast::VarDecl(_, ident_tkn, _) => {
                let dest = &self.reg_pool.alloc();
                GenResult {
                    instrs: vec![
                        OpCode::MvVal(dest.to_string(), 0.0),
                        OpCode::St(ident_tkn.get_name(), dest.to_string())
                    ],
                    dest_name: dest.to_string()
                }
            },
            Ast::VarAssign(_, ident_tkn, _, maybe_assign_ast) => {
                let ast = maybe_assign_ast.clone().unwrap();
                let mut gen_assign = self.gen_expr(&ast);
                let storage_location = ident_tkn.get_name();

                let mut st_op = vec![OpCode::St(ident_tkn.get_name(), gen_assign.dest_name)];
                gen_assign.instrs.append(&mut st_op);

                GenResult {
                    instrs: gen_assign.instrs,
                    dest_name: storage_location
                }
            },
            _ => unimplemented!("{:?}", stmt)
        }
    }

    fn gen_expr(&mut self, expr: &Ast) -> GenResult {
        match expr {
            Ast::Primary(ty_rec) => {
                let dest_reg = self.reg_pool.alloc();
                GenResult {
                    instrs: vec![self.get_primary_ast_op(expr, &dest_reg)],
                    dest_name: dest_reg
                }
            },
            Ast::Unary(op_tkn, maybe_rhs) => {
                let rhs = maybe_rhs.clone().unwrap();
                self.gen_unary_op(op_tkn, &rhs)
            },
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs = maybe_lhs.clone().unwrap();
                let rhs = maybe_rhs.clone().unwrap();
                self.gen_bin_op(op_tkn, &lhs, &rhs)
            },
            Ast::VarAssign(_,_,_,_) => {
                self.gen_stmt(expr)
            }
            _ => unimplemented!("{:?}", expr)
        }
    }

    fn gen_unary_op(&mut self, op_tkn: &Token, rhs: &Ast) -> GenResult {
        let mut rhs_gen_result = self.gen_expr(rhs);
        let op = match op_tkn.ty {
            TknTy::Bang => {
                OpCode::Not(rhs_gen_result.dest_name.clone(), rhs_gen_result.dest_name.clone())
            },
            TknTy::Minus => {
                OpCode::Neg(rhs_gen_result.dest_name.clone(), rhs_gen_result.dest_name.clone())
            },
            _ => panic!("Invalid unary operator found!")
        };

        rhs_gen_result.instrs.append(&mut vec![op]);
        GenResult {
            instrs: rhs_gen_result.instrs,
            dest_name: rhs_gen_result.dest_name
        }
    }

    fn gen_bin_op(&mut self, op_tkn: &Token, lhs: &Ast, rhs: &Ast) -> GenResult {
        if lhs.is_primary() && rhs.is_primary() {
            let lhs_reg = self.reg_pool.alloc();
            let rhs_reg = self.reg_pool.alloc();

            let ops = vec![
                self.get_primary_ast_op(lhs, &lhs_reg),
                self.get_primary_ast_op(rhs, &rhs_reg),
                self.get_bin_op_ty(op_tkn, &lhs_reg, &rhs_reg)
            ];

            return GenResult {
                instrs: ops,
                dest_name: lhs_reg
            };
        } else if lhs.is_primary() && !rhs.is_primary() {
            let lhs_reg = self.reg_pool.alloc();
            let lhs_op = self.get_primary_ast_op(lhs, &lhs_reg);

            let mut rhs_gen_result = self.gen_expr(rhs);
            let prev_dest_reg = rhs_gen_result.dest_name;
            let op = self.get_bin_op_ty(op_tkn, &lhs_reg, &prev_dest_reg);
            let mut instrs = vec![lhs_op, op];

            rhs_gen_result.instrs.append(&mut instrs);
            return GenResult {
                instrs: rhs_gen_result.instrs,
                dest_name: prev_dest_reg
            };
        } else if !lhs.is_primary() && rhs.is_primary() {
            let rhs_reg = self.reg_pool.alloc();
            let rhs_op =  self.get_primary_ast_op(rhs, &rhs_reg);

            let mut lhs_gen_result = self.gen_expr(lhs);
            let prev_dest_reg = lhs_gen_result.dest_name;
            let op = self.get_bin_op_ty(op_tkn, &prev_dest_reg, &rhs_reg);
            let mut instrs = vec![rhs_op, op];

            lhs_gen_result.instrs.append(&mut instrs);
            return GenResult {
                instrs: lhs_gen_result.instrs,
                dest_name: prev_dest_reg
            };
        } else {
            let mut lhs_gen_result = self.gen_expr(lhs);
            let mut rhs_gen_result = self.gen_expr(rhs);
            let op = self.get_bin_op_ty(op_tkn, &lhs_gen_result.dest_name, &rhs_gen_result.dest_name);

            lhs_gen_result.instrs.append(&mut rhs_gen_result.instrs);
            let mut instrs = vec![op];
            lhs_gen_result.instrs.append(&mut instrs);

            return GenResult {
                instrs: lhs_gen_result.instrs,
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

    fn get_primary_ast_op(&self, ast: &Ast, dest_reg: &str) -> OpCode {
        match ast {
            Ast::Primary(ty_rec) => {
                match ty_rec.tkn.ty {
                    TknTy::Ident(ref name) => {
                        OpCode::Ld(dest_reg.to_string(), name.to_string())
                    },
                    TknTy::Val(v) => {
                        OpCode::MvVal(dest_reg.to_string(), v)
                    },
                    TknTy::True => {
                        OpCode::MvVal(dest_reg.to_string(), 1.0)
                    },
                    TknTy::False => {
                        OpCode::MvVal(dest_reg.to_string(), 0.0)
                    },
                    _ => panic!("Invalid primary token found")
                }
            },
            _ => panic!("Cannot get primary op from non-primary ast")
        }
    }
}
