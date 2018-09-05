use ast::Ast;
use symtab::SymbolTable;
use sym::{Sym, SymTy};
use lexer::Lexer;
use token::{Token, TknTy};
use type_record::TyRecord;
use errors::ErrC;

const FN_PARAM_MAX_LEN: usize = 64;

pub struct ParserResult {
    /// The resulting AST from parsing
    pub ast: Option<Box<Ast>>,

    /// Vector of any parser errors
    pub error: Vec<ErrC>
}

impl ParserResult {
    pub fn new() -> ParserResult {
        ParserResult {
            ast: None,
            error: Vec::new()
        }
    }
}

pub struct Parser<'l, 's> {
    /// Reference to the lexer needed to get characters from the file
    lexer: &'l mut Lexer,
    symtab: &'s mut SymbolTable,
    errors: Vec<ErrC>,
    currtkn: Token
}

impl<'l, 's> Parser<'l, 's> {
    pub fn new(lex: &'l mut Lexer, symt: &'s mut SymbolTable) -> Parser<'l, 's> {
        let firsttkn = lex.lex();

        Parser {
            lexer: lex,
            symtab: symt,
            errors: Vec::new(),
            currtkn: firsttkn
        }
    }

    /// Main entry point to the recursive descent parser. Calling this method will parse the entire
    /// file and return a result containing the AST and any parsing errors encountered.
    /// The error vector should be checked after parsing, and any errors should
    /// be handled before continuing to future compiler passes.
    pub fn parse(&mut self) -> ParserResult {
        let mut stmts: Vec<Ast> = Vec::new();

        while self.currtkn.ty != TknTy::Eof {
            match self.decl() {
                Some(a) => stmts.push(a),
                None => ()
            }
        }

        // Finalize the global scope to access scopes in future passes.
        self.symtab.finalize_global_sc();

        let head = Ast::Prog{stmts: stmts};
        ParserResult {
            ast: Some(Box::new(head)),
            error: self.errors.clone()
        }
    }

    /// Parses a declaration. In kolga we can declare variables, functions, and classes.
    fn decl(&mut self) -> Option<Ast> {
        match self.currtkn.ty {
            TknTy::Let => self.var_decl(),
            TknTy::Func => self.func_decl(),
            TknTy::Class => self.class_decl(),
            _ => self.stmt()
        }
    }

    /// Parses a variable declaration
    fn var_decl(&mut self) -> Option<Ast> {
        self.expect(TknTy::Let);

        let is_imm = match self.currtkn.ty {
            TknTy::Imm => {
                self.consume();
                true
            },
            _ => false
        };

        let ident_tkn = self.match_ident_tkn();
        if ident_tkn.is_none() {
            return None;
        }

        self.expect(TknTy::Tilde);
        let mut is_class_type = false;

        let var_ty_tkn = if self.currtkn.is_ty() {
            let tkn = Some(self.currtkn.clone());
            self.consume();
            tkn
        } else {
            let ty_name = self.currtkn.get_name();
            let maybe_class_sym = self.symtab.retrieve(&ty_name);
            if maybe_class_sym.is_none() {
                let err_msg = format!("{:?} is not a valid type", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                None
            } else if maybe_class_sym.unwrap().sym_ty == SymTy::Class {
                is_class_type = true;
                let tkn = Some(self.currtkn.clone());
                self.consume();
                tkn
            } else {
                let err_msg = format!("{:?} is not a valid type", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                None
            }
        };

        if var_ty_tkn.is_none() {
            return None;
        }

        match self.currtkn.ty {
            TknTy::Eq => {
                self.consume();
                let var_val = self.expr();
                self.expect(TknTy::Semicolon);

                let ty_rec = TyRecord::new_from_tkn(var_ty_tkn.unwrap());
                let sym = Sym::new(SymTy::Var,
                                   is_imm,
                                   ty_rec.clone(),
                                   ident_tkn.clone().unwrap(),
                                   var_val.clone(),
                                   None);

                let name = &ident_tkn.clone().unwrap().get_name();
                self.symtab.store(name, sym);

                Some(Ast::VarAssign{
                    ty_rec: ty_rec,
                    ident_tkn: ident_tkn.unwrap(),
                    is_imm: is_imm,
                    is_global: self.symtab.is_global(),
                    value: Box::new(var_val)
                })
            },
            TknTy::Semicolon => {
                if is_imm {
                    let err_msg = format!("Cannot declare immutable variable with no value");
                    self.err_from_tkn(err_msg);
                    return None;
                }
                self.consume();

                if is_class_type {
                    let class_sym = self.symtab.retrieve(&var_ty_tkn.clone().unwrap().get_name()).unwrap();
                    let cl_ty_rec = TyRecord::new_from_tkn(var_ty_tkn.clone().unwrap());
                    let cl_assign = class_sym.assign_val.clone();
                    let cl_sym = Sym::new(SymTy::Var,
                                          is_imm,
                                          cl_ty_rec.clone(),
                                          ident_tkn.clone().unwrap(),
                                          cl_assign.clone(),
                                          None);

                    let name = &ident_tkn.clone().unwrap().get_name();
                    self.symtab.store(name, cl_sym);

                    return Some(Ast::VarAssign{
                        ty_rec: cl_ty_rec,
                        ident_tkn: ident_tkn.clone().unwrap(),
                        is_imm: is_imm,
                        is_global: self.symtab.is_global(),
                        value: Box::new(cl_assign)
                    });
                }

                let ty_rec = TyRecord::new_from_tkn(var_ty_tkn.unwrap());
                let sym = Sym::new(SymTy::Var,
                                   is_imm,
                                   ty_rec.clone(),
                                   ident_tkn.clone().unwrap(),
                                   None,
                                   None);

                let name = &ident_tkn.clone().unwrap().get_name();
                self.symtab.store(name, sym);

                Some(Ast::VarDecl{
                    ty_rec: ty_rec,
                    ident_tkn: ident_tkn.unwrap(),
                    is_imm: is_imm,
                    is_global: self.symtab.is_global()
                })
            },
            _ => {
                let err_msg = format!("Invalid var declaration: {:?}", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                None
            }
        }
    }

    fn func_decl(&mut self) -> Option<Ast> {
        self.expect(TknTy::Func);
        let func_ident_tkn = self.currtkn.clone();
        self.consume();

        let mut params = Vec::new();
        self.expect(TknTy::LeftParen);

        // Create function level scope
        self.symtab.init_sc();

        while self.currtkn.ty != TknTy::RightParen {
            if params.len() > FN_PARAM_MAX_LEN {
                let err_msg = format!("Function param count exceeded limit of {}", FN_PARAM_MAX_LEN);
                self.err_from_tkn(err_msg);
                return None;
            }

            let ident_tkn = self.currtkn.clone();
            self.consume();
            self.expect(TknTy::Tilde);

            let mut ty_rec = TyRecord::new_from_tkn(self.currtkn.clone());
            ty_rec.tkn = ident_tkn.clone();

            params.push(ty_rec.clone());

            // Store param variable name in the symbol table for the function scope.
            let param_sym = Sym::new(SymTy::Param, false, ty_rec, ident_tkn.clone(), None, None);
            self.symtab.store(&ident_tkn.get_name(), param_sym);

            self.consume();
            if self.currtkn.ty == TknTy::RightParen {
                break;
            }
            self.expect(TknTy::Comma);
        }

        self.expect(TknTy::RightParen);
        self.expect(TknTy::Tilde);

        let fn_ret_ty_tkn = match self.currtkn.ty {
            TknTy::String | TknTy::Num | TknTy::Bool => {
                let tkn = self.currtkn.clone();
                self.consume();
                Some(tkn)
            },
            _ => {
                let err_msg = format!("Token is not a valid type: {:?}", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                None
            }
        };

        if fn_ret_ty_tkn.is_none() {
            return None;
        }

        let fn_ty_rec = TyRecord::new_from_tkn(fn_ret_ty_tkn.clone().unwrap());
        let fn_body = self.block_stmt();

        // After parsing the body, we close the function block scope.
        let sc_idx = self.symtab.finalize_sc();

        let fn_sym = Sym::new(SymTy::Func,
                              true,
                              fn_ty_rec.clone(),
                              func_ident_tkn.clone(),
                              fn_body.clone(),
                              Some(params.clone()));

        let name = &func_ident_tkn.get_name();
        self.symtab.store(name, fn_sym);

        Some(Ast::FuncDecl {
            ident_tkn: func_ident_tkn,
            params: params,
            ret_ty: fn_ty_rec,
            func_body: Box::new(fn_body),
            scope_lvl: sc_idx
        })
    }

    /// Parses a class declaration
    fn class_decl(&mut self) -> Option<Ast> {
        self.expect(TknTy::Class);
        let class_tkn = self.currtkn.clone();
        self.consume();
        self.expect(TknTy::LeftBrace);

        // TODO: open another scope here for class level declarations
        // TODO: class decl needs a finalized scope in the ast

        let mut methods = Vec::new();
        let mut props = Vec::new();

        loop {
            match self.currtkn.ty {
                TknTy::Let => props.push(self.var_decl()),
                TknTy::Func => methods.push(self.func_decl()),
                TknTy::RightBrace => {
                    self.consume();
                    break;
                },
                _ => {
                    let err_msg = format!("Invalid token in class declaration: {:?}", self.currtkn.ty);
                    self.err_from_tkn(err_msg);
                    break;
                }
            }
        }

        let ast = Some(Ast::ClassDecl {
            ident_tkn: class_tkn.clone(),
            methods: methods,
            props: props
        });

        let sym = Sym::new(SymTy::Class,
                           true,
                           TyRecord::new_from_tkn(class_tkn.clone()),
                           class_tkn.clone(),
                           ast.clone(),
                           None);
        self.symtab.store(&class_tkn.get_name(), sym);

        ast
    }

    /// Parses a statement. This function does not perform any scope management, which
    /// is delegated to each statement type.
    fn stmt(&mut self) -> Option<Ast> {
        match self.currtkn.ty {
            TknTy::If => self.if_stmt(),
            TknTy::While => self.while_stmt(),
            TknTy::For => self.for_stmt(),
            TknTy::Return => self.ret_stmt(),
            TknTy::LeftBrace => self.block_stmt(),
            _ => self.expr_stmt()
        }
    }

    /// Parses a block statement, beginning with a '{' token. This creates a new scope,
    /// parses any statements within the block, and closes the block scope at the end.
    fn block_stmt(&mut self) -> Option<Ast> {
        self.expect(TknTy::LeftBrace);
        let mut stmts = Vec::new();
        self.symtab.init_sc();

        loop {
            match self.currtkn.ty {
                TknTy::RightBrace | TknTy::Eof => break,
                _ => stmts.push(self.decl())
            };
        }

        self.expect(TknTy::RightBrace);
        let sc_lvl = self.symtab.finalize_sc();

        Some(Ast::BlckStmt{
            stmts: stmts,
            scope_lvl: sc_lvl
        })
    }

    /// Parse an if statement, including else and elif blocks. These are stored in the
    /// IfStmt Ast type.
    fn if_stmt(&mut self) -> Option<Ast> {
        self.expect(TknTy::If);
        let maybe_if_cond = self.expr();
        if maybe_if_cond.is_none() {
            return None;
        }

        let maybe_if_blck = self.block_stmt();
        let mut maybe_else_blck = None;
        let mut else_ifs = Vec::new();
        loop {
            match self.currtkn.ty {
                TknTy::Elif => {
                    self.consume();
                    let maybe_elif_ast = self.expr();
                    let maybe_elif_blck = self.block_stmt();
                    else_ifs.push(Some(Ast::ElifStmt(Box::new(maybe_elif_ast), Box::new(maybe_elif_blck))));
                },
                TknTy::Else => {
                    self.consume();
                    maybe_else_blck = self.block_stmt();
                },
                _ => break
            };
        }

        Some(Ast::IfStmt(Box::new(maybe_if_cond),
                     Box::new(maybe_if_blck),
                     else_ifs,
                     Box::new(maybe_else_blck)))
    }

    fn while_stmt(&mut self) -> Option<Ast> {
        self.expect(TknTy::While);
        // TODO: skip expr for infinite loop when we have a break stmt
        let maybe_while_cond = self.expr();
        if maybe_while_cond.is_none() {
            return None;
        }

        let while_stmts = self.block_stmt();
        Some(Ast::WhileStmt(Box::new(maybe_while_cond), Box::new(while_stmts)))
    }

    fn for_stmt(&mut self) -> Option<Ast> {
        self.expect(TknTy::For);
        let mut for_var_decl = None;
        let mut for_var_cond = None;
        let mut for_incr_expr = None;

        match self.currtkn.ty {
            TknTy::Semicolon => self.consume(),
            TknTy::Let => {
                for_var_decl = self.var_decl();
            },
            _ => {
                let err_msg = String::from("Invalid for statement: Must start with a var declaration");
                self.err_from_tkn(err_msg);
            }
        };

        match self.currtkn.ty {
            TknTy::Semicolon => self.consume(),
            _ => {
                for_var_cond = self.expr_stmt();
            }
        };

        match self.currtkn.ty {
            TknTy::Semicolon => self.consume(),
            _ => {
                for_incr_expr = self.expr_stmt();
            }
        };

        let for_stmt = self.block_stmt();

        Some(Ast::ForStmt{
            for_var_decl: Box::new(for_var_decl),
            for_cond_expr: Box::new(for_var_cond),
            for_step_expr: Box::new(for_incr_expr),
            stmts: Box::new(for_stmt)
        })
    }

    fn ret_stmt(&mut self) -> Option<Ast> {
        self.expect(TknTy::Return);
        match self.currtkn.ty {
            TknTy::Semicolon => {
                self.consume();
                Some(Ast::RetStmt(Box::new(None)))
            },
            _ => {
                let maybe_ret_expr = self.expr();
                self.expect(TknTy::Semicolon);
                Some(Ast::RetStmt(Box::new(maybe_ret_expr)))
            }
        }
    }

    fn expr_stmt(&mut self) -> Option<Ast> {
        let maybe_expr = self.expr();
        self.expect(TknTy::Semicolon);
        Some(Ast::ExprStmt(Box::new(maybe_expr)))
    }

    fn expr(&mut self) -> Option<Ast> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Option<Ast> {
        let maybe_ast = self.logicor_expr();
        if maybe_ast.is_none() {
            return None;
        }

        match self.currtkn.ty {
            TknTy::Eq => {
                let op = self.currtkn.clone();
                self.consume();
                let rhs = self.assign_expr();

                match maybe_ast.clone().unwrap() {
                    Ast::Primary(tyrec) => {
                        match tyrec.tkn.ty {
                            TknTy::Ident(name) => {
                                let maybe_sym = self.symtab.retrieve(&name);
                                if maybe_sym.is_none() {
                                    let err_msg = format!("Undeclared variable {:?} found, cannot assign",
                                                          name);
                                    self.err_from_tkn(err_msg);
                                    return None;
                                }

                                let sym = maybe_sym.unwrap();

                                if sym.imm {
                                    let err_msg = format!("Cannot re-assign immutable variable");
                                    self.err_from_tkn(err_msg);
                                    return None;
                                }

                                return Some(Ast::VarAssign{
                                    ty_rec: sym.ty_rec.clone(),
                                    ident_tkn: sym.ident_tkn.clone(),
                                    is_imm: sym.imm,
                                    is_global: self.symtab.is_global(),
                                    value: Box::new(rhs)
                                });
                            },
                            _ => {
                                let err_msg = format!("Token {:?} is invalid for assignment",
                                                      tyrec.tkn.ty.clone());
                                self.err_from_tkn(err_msg);
                                return None;
                            }
                        };
                    },
                    Ast::ClassGet(cls, prop) => {
                        return Some(Ast::ClassSet(cls, prop, Box::new(rhs)));
                    },
                    _ => {
                        let err_msg = format!("Token {:?} is invalid for assignment", op.ty);
                        let error = ErrC::new(op.line, op.pos, err_msg);
                        self.errors.push(error);
                    }
                }
            },
            _ => ()
        };

        maybe_ast
    }

    fn logicor_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.logicand_expr();
        if maybe_ast.is_none() {
            return None;
        }

        loop {
            match self.currtkn.ty {
                TknTy::PipePipe | TknTy::Or => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.logicand_expr();
                    maybe_ast = Some(Ast::Logical(op, Box::new(maybe_ast), Box::new(rhs)));
                },
                _ => break
            }
        }

        maybe_ast
    }

    fn logicand_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.eq_expr();
        if maybe_ast.is_none() {
            return None;
        }

        loop {
            match self.currtkn.ty {
                TknTy::AmpAmp | TknTy::And => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.eq_expr();
                    maybe_ast = Some(Ast::Logical(op, Box::new(maybe_ast), Box::new(rhs)));
                },
                _ => break
            }
        }

        maybe_ast
    }

    fn eq_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.cmp_expr();
        if maybe_ast.is_none() {
            return None;
        }

        loop {
            match self.currtkn.ty {
                TknTy::BangEq | TknTy::EqEq => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.cmp_expr();
                    maybe_ast = Some(Ast::Binary(op, Box::new(maybe_ast), Box::new(rhs)));
                },
                _ => break
            }
        }

        maybe_ast
    }

    fn cmp_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.addsub_expr();
        if maybe_ast.is_none() {
            return None;
        }

        loop {
            match self.currtkn.ty {
                TknTy::Lt | TknTy::LtEq | TknTy::Gt | TknTy::GtEq => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.addsub_expr();
                    maybe_ast = Some(Ast::Binary(op, Box::new(maybe_ast), Box::new(rhs)));
                },
                _ => break
            }
        }

        maybe_ast
    }

    fn addsub_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.muldiv_expr();
        if maybe_ast.is_none() {
            return None;
        }

        loop {
            match self.currtkn.ty {
                TknTy::Plus | TknTy::Minus => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.muldiv_expr();
                    maybe_ast = Some(Ast::Binary(op, Box::new(maybe_ast), Box::new(rhs)));
                },
                _ => break
            }
        }

        maybe_ast
    }

    fn muldiv_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.unary_expr();
        if maybe_ast.is_none() {
            return None;
        }

        loop {
            match self.currtkn.ty {
                TknTy::Star | TknTy::Slash => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.unary_expr();
                    maybe_ast = Some(Ast::Binary(op, Box::new(maybe_ast), Box::new(rhs)))
                },
                _ => break
            }
        }

        maybe_ast
    }

    fn unary_expr(&mut self) -> Option<Ast> {
        match self.currtkn.ty {
            TknTy::Bang | TknTy::Minus => {
                let op = self.currtkn.clone();
                self.consume();
                let rhs = self.unary_expr();
                return Some(Ast::Unary(op, Box::new(rhs)));
            },
            _ => self.fncall_expr()
        }
    }

    fn fncall_expr(&mut self) -> Option<Ast> {
        let mut maybe_ast = self.primary_expr();
        if maybe_ast.is_none() {
            return None;
        }

        let func_name_tkn = match maybe_ast.clone().unwrap() {
            Ast::Primary(tyrec) => Some(tyrec.tkn),
            _ => None
        };

        // TODO: loop this?
        match self.currtkn.ty {
            TknTy::LeftParen => {
                maybe_ast = self.fnparams_expr(func_name_tkn);
            },
            TknTy::Period => {
                // calling a method on a class or getting a property
                self.consume();
                let name = self.match_ident_tkn();
                match self.currtkn.ty {
                    TknTy::LeftParen => {
                        let fn_ast = self.fnparams_expr(name.clone());
                        let params = fn_ast.unwrap().extract_params();
                        maybe_ast = Some(Ast::ClassFnCall(func_name_tkn.clone().unwrap(),
                                                          name.unwrap().clone(),
                                                          params));
                    },
                    _ => {
                        maybe_ast = Some(Ast::ClassGet(func_name_tkn, name));
                    }
                }
            },
            _ => ()
        };

        maybe_ast
    }

    fn fnparams_expr(&mut self, fn_tkn: Option<Token>) -> Option<Ast> {
        self.expect(TknTy::LeftParen);

        let fn_sym = self.symtab.retrieve(&fn_tkn.clone().unwrap().get_name());

        if fn_sym.is_none() {
            let tkn = fn_tkn.clone().unwrap();
            let err_msg = format!("Function call: Undeclared symbol {:?} found",
                                  tkn.get_name());
            let error = ErrC::new(tkn.line, tkn.pos, err_msg);
            self.errors.push(error);
        }

        let mut params: Vec<Ast> = Vec::new();
        while self.currtkn.ty != TknTy::RightParen {
            if params.len() > FN_PARAM_MAX_LEN {
                let err_msg = format!("Function param count exceeded limit of {}", FN_PARAM_MAX_LEN);
                self.err_from_tkn(err_msg);
                return None;
            }

            match self.expr() {
                Some(a) => params.push(a),
                None => ()
            };

            if self.currtkn.ty == TknTy::RightParen {
                break;
            }
            self.expect(TknTy::Comma);
        }

        self.expect(TknTy::RightParen);

        let expected_params = fn_sym.clone().unwrap().fn_params.clone().unwrap();
        if expected_params.len() != params.len() {
            // TODO: could pass in a list here and print the list instead of the counts
            let err_msg = format!("Incorrect function parameters: Expected {} arguments, but found {}.",
                                  expected_params.len(),
                                  params.len());
            let tkn = fn_tkn.clone().unwrap();
            let error = ErrC::new(tkn.line, tkn.pos, err_msg);
            self.errors.push(error);
        }

        Some(Ast::FnCall(fn_tkn, params))
    }

    fn primary_expr(&mut self) -> Option<Ast> {
        match self.currtkn.ty.clone() {
            TknTy::Str(_) |
            TknTy::Val(_) |
            TknTy::True |
            TknTy::False |
            TknTy::Null => {
                let ast = Some(Ast::Primary(TyRecord::new_from_tkn(self.currtkn.clone())));
                self.consume();
                ast
            },
            TknTy::Ident(ref ident_name) => {
                let mb_sym = self.symtab.retrieve(ident_name);
                if mb_sym.is_none() {
                    let msg = format!("Undeclared variable {} found", ident_name);
                    self.err_from_tkn(msg);
                    self.consume();
                    return None;
                }

                let sym = mb_sym.unwrap();
                if sym.assign_val.is_none() && sym.sym_ty != SymTy::Param {
                    let next_tkn = self.lexer.peek_tkn();
                    // If the following token is '=', we don't need to report an error
                    // for unitialized var (we are initializing it here).
                    if next_tkn.ty != TknTy::Eq {
                        let msg = format!("Cannot use un-assigned variable {}", ident_name);
                        self.err_from_tkn(msg);
                        self.consume();
                        return None;
                    }
                }

                let mut ty_rec = sym.ty_rec.clone();
                ty_rec.tkn = self.currtkn.clone();
                let ast = Some(Ast::Primary(ty_rec.clone()));
                self.consume();
                ast
            },
            TknTy::LeftParen => {
                self.consume();
                let ast = self.expr();
                self.expect(TknTy::RightParen);
                ast
            },
            TknTy::String | TknTy::Num | TknTy::Bool => {
                let err_msg = format!("Cannot assign reserved type '{:?}' to variable", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                self.consume();
                None
            },
            _ => {
                let err_msg = format!("Unknown token {:?} found", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                self.consume();
                None
            }
        }
    }

    fn match_ident_tkn(&mut self) -> Option<Token> {
        match self.currtkn.ty {
            TknTy::Ident(_) => {
                let tkn = Some(self.currtkn.clone());
                self.consume();
                tkn
            },
            _ => {
                let err_msg = format!("{:?} token is not a valid identifier", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                None
            }
        }
    }

    /// Check that the current token is the same as the one we expect. If it is, consume the
    /// token and advance. If it isn't report an error.
    fn expect(&mut self, tknty: TknTy) {
        let err_msg = format!("Expected token '{:?}', but found '{:?}'", tknty, self.currtkn.ty);
        if self.currtkn.ty == tknty {
            self.consume()
        } else {
            self.err_from_tkn(err_msg)
        }
    }

    fn consume(&mut self) {
        self.currtkn = self.lexer.lex();
    }

    fn err_from_tkn(&mut self, message: String) {
        let prefixed_msg = format!("Parse Error - {}", message);
        let error = ErrC::new(self.currtkn.line, self.currtkn.pos, prefixed_msg);
        self.errors.push(error);
    }
}
