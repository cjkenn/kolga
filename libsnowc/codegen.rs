use ast::Ast;
use token::{TknTy, Token};
use symtab::SymTab;
use vm::reg::RegPool;
use vm::op::OpCode;

pub struct CodeGen<'c, 's> {
    ast: &'c Ast,
    symtab: &'s SymTab,
    reg_pool: &'s mut RegPool,
    ops: Vec<OpCode>
}

impl <'c, 's> CodeGen<'c, 's> {
    pub fn new(ast: &'c Ast, symtab: &'s SymTab, regpool: &'s mut RegPool) -> CodeGen<'c, 's> {
        CodeGen {
            ast: ast,
            symtab: symtab,
            reg_pool: regpool,
            ops: Vec::new()
        }
    }

    pub fn gen(&mut self) -> Vec<OpCode> {
        match *self.ast {
            Ast::Prog(ref stmts) => {
                for stmt in stmts {
                    self.gen_stmt(&stmt);
                }
            },
            _ => ()
        };

        self.ops.clone()
    }

    fn gen_stmt(&mut self, stmt: &Ast) {
        match stmt {
            Ast::ExprStmt(maybe_ast) => {
                let ast = maybe_ast.clone().unwrap();
                self.gen_expr(&ast);
            },
            _ => unimplemented!()
        }
    }

    fn gen_expr(&mut self, expr: &Ast) {
        match expr {
            Ast::Binary(op_tkn, maybe_lhs, maybe_rhs) => {
                let lhs = maybe_lhs.clone().unwrap();
                let rhs = maybe_rhs.clone().unwrap();
                self.gen_bin_op(op_tkn, &lhs, &rhs);
            },
            _ => unimplemented!()
        }
    }

    fn gen_bin_op(&mut self, op_tkn: &Token, lhs: &Ast, rhs: &Ast) {
        match op_tkn.ty {
            TknTy::Plus => {
                // TODO: assuming two literals for now (both sides are Ast::Primary)
                let l_val = lhs.extract_primary_ty_rec().tkn.get_val();
                let r_val = rhs.extract_primary_ty_rec().tkn.get_val();

                let l_reg = self.reg_pool.alloc();
                let r_reg = self.reg_pool.alloc();
                let dest = self.reg_pool.alloc();

                let l_op = OpCode::MvVal(l_reg.clone(), l_val);
                let r_op = OpCode::MvVal(r_reg.clone(), r_val);
                let op = OpCode::Add(dest, l_reg, r_reg);

                let mut add_ops = vec![l_op, r_op, op];

                self.ops.append(&mut add_ops);
            },
            TknTy::Minus => {

            },
            TknTy::Star => {

            }
            TknTy::Slash => {

            },
            _ => unimplemented!("Invalid binary operator found in gen")
        }
    }
}
