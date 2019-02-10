use ast::{Ast, MetaAst};
use error::parse::{ParseErr, ParseErrTy};
use error::KolgaErr;
use lexer::Lexer;
use std::collections::HashMap;
use std::rc::Rc;
use sym::{Sym, SymTy};
use symtab::SymbolTable;
use token::{TknTy, Token};
use ty_rec::{KolgaTy, TyRecord};

const FN_PARAM_MAX_LEN: usize = 64;

/// ParserResult handles the result from parsing a file. This contains an optional
/// AST structure, as well as a flag indicating whether or not continuable errors
/// were encountered during the parsing phase. This is returned from the parse()
/// method, and should be checked for errors before continuing further phases
/// of the compiler.
pub struct ParserResult {
    /// The resulting AST from parsing.
    pub ast: Option<Ast>,

    /// Flag indicating if errors have ocurred during parsing.
    pub has_err: bool,
}

impl ParserResult {
    pub fn new() -> ParserResult {
        ParserResult {
            ast: None,
            has_err: false,
        }
    }
}

struct ParseContext {
    pub class_ctx: Option<ClassContext>,
}

impl ParseContext {
    pub fn new() -> ParseContext {
        ParseContext { class_ctx: None }
    }
}

#[derive(Clone, Debug)]
pub struct ClassContext {
    pub prop_map: HashMap<String, usize>,
    pub methods: Vec<Ast>,
}

impl ClassContext {
    pub fn new(pm: HashMap<String, usize>, mtods: Vec<Ast>) -> ClassContext {
        ClassContext {
            prop_map: pm,
            methods: mtods,
        }
    }
}

pub struct Parser<'l, 's> {
    /// Reference to the lexer needed to get characters from the file.
    lexer: &'l mut Lexer,

    /// Reference to a symbol table, used to store symbols defined in this file.
    symtab: &'s mut SymbolTable,

    /// Vector of errors. We continue to store errors because we may encounter
    /// continuable errors, where we keep parsing but we need to report the
    /// errors later.
    // TODO: this should probably go into the ParserResult struct
    errors: Vec<ParseErr>,

    /// The current token from the lexer.
    currtkn: Token,

    /// AST node size.
    node_count: usize,

    /// Number of symbols encountered.
    sym_count: usize,
}

impl<'l, 's> Parser<'l, 's> {
    pub fn new(lex: &'l mut Lexer, symt: &'s mut SymbolTable) -> Parser<'l, 's> {
        let firsttkn = lex.lex();

        Parser {
            lexer: lex,
            symtab: symt,
            errors: Vec::new(),
            currtkn: firsttkn,
            node_count: 1, // start at 1 because the entry node always has id 0
            sym_count: 0,
        }
    }

    /// Main entry point to the recursive descent parser. Calling this method will parse the entire
    /// file and return a result containing the AST and any parsing errors encountered.
    /// The error vector should be checked after parsing, and any errors should
    /// be handled before continuing to future compiler passes.
    pub fn parse(&mut self) -> ParserResult {
        let mut stmts: Vec<Ast> = Vec::new();
        let mut found_err = false;

        // Create a new empty parse context, whose reference is passed to all
        // parsing functions
        let mut pctx = ParseContext::new();

        while self.currtkn.ty != TknTy::Eof {
            match self.decl(&pctx) {
                Ok(a) => stmts.push(a),
                Err(e) => {
                    found_err = true;
                    e.emit();
                    match e.continuable() {
                        true => (),
                        false => break,
                    };
                }
            }
        }

        // Finalize the global scope to access scopes in future passes.
        self.symtab.finalize_global_sc();

        let head = Ast::Prog {
            meta: MetaAst::new(0, 0, 0),
            stmts: stmts,
        };

        ParserResult {
            ast: Some(head),
            has_err: found_err,
        }
    }

    /// Parses a declaration. In kolga we can declare variables, functions, and classes.
    fn decl(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        match self.currtkn.ty {
            TknTy::Let => self.var_decl(pctx),
            TknTy::Fn => self.fn_decl(pctx),
            TknTy::Class => self.class_decl(pctx),
            _ => self.stmt(pctx),
        }
    }

    /// Parses a variable declaration
    fn var_decl(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        self.expect(TknTy::Let)?;

        let is_imm = match self.currtkn.ty {
            TknTy::Imm => {
                self.consume();
                true
            }
            _ => false,
        };

        let ident_tkn = self.match_ident_tkn();

        self.expect(TknTy::Tilde)?;

        let mut is_class_type = false;
        let mut var_err = None;
        let mut var_ty_tkn = None;

        // Check for a type annotation. If there is one, set var_ty_tkn so we can use it later
        // to create type records. If there is no type, we will try to infer one later. If the
        // type annotation is a class identifier, we parse that to make sure the class
        // actually exists and is valid to use as a type.
        if self.currtkn.is_ty() {
            // Void isn't a valid type for a variable, just a function that returns nothing
            if self.currtkn.ty == TknTy::Void {
                let ty_str = self.currtkn.ty.to_string();
                return Err(self.error(ParseErrTy::InvalidTy(ty_str)));
            }

            let tkn = Some(self.currtkn.clone());
            self.consume();
            var_ty_tkn = tkn;
        } else if self.currtkn.ty == TknTy::Eq || self.currtkn.ty == TknTy::Semicolon {
            var_ty_tkn = None;
        } else {
            let ty_name = self.currtkn.get_name();
            let maybe_class_sym = self.symtab.retrieve(&ty_name);

            if maybe_class_sym.is_none() {
                let ty_str = self.currtkn.ty.to_string();
                var_err = Some(self.error(ParseErrTy::InvalidTy(ty_str)));
                var_ty_tkn = None;
            } else if maybe_class_sym.unwrap().sym_ty == SymTy::Class {
                is_class_type = true;
                let tkn = Some(self.currtkn.clone());
                self.consume();
                var_ty_tkn = tkn;
            } else {
                let ty_str = self.currtkn.ty.to_string();
                var_err = Some(self.error(ParseErrTy::InvalidTy(ty_str)));
                var_ty_tkn = None;
            }
        };

        if var_ty_tkn.is_none() && var_err.is_some() {
            return Err(var_err.unwrap());
        }

        match self.currtkn.ty {
            TknTy::Eq => {
                // If we don't have a type tkn, we need to infer it. So, we create our type
                // record with a symbolic type instead.
                let ty_rec = match var_ty_tkn {
                    None => TyRecord::unknown(self.currtkn.clone(), self.next_sym()),
                    Some(tkn) => TyRecord::new(tkn.clone(), self.next_sym()),
                };

                self.consume();
                let var_val = self.expr(pctx)?;
                self.expect(TknTy::Semicolon)?;

                let sym = Sym::new(
                    SymTy::Var,
                    is_imm,
                    ty_rec.clone(),
                    ident_tkn.clone().unwrap(),
                    Some(var_val.clone()),
                    None,
                );

                let name = &ident_tkn.clone().unwrap().get_name();
                self.symtab.store(name, sym);
                let tkn = ident_tkn.clone().unwrap();

                Ok(Ast::VarAssignExpr {
                    meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
                    ty_rec: ty_rec,
                    ident_tkn: ident_tkn.unwrap(),
                    is_imm: is_imm,
                    is_global: self.symtab.is_global(),
                    value: Box::new(var_val),
                })
            }
            TknTy::Semicolon => {
                // Check if we're trying to create an immutable variable but with no
                // value assigned to it. We treat this as an error, since we should never
                // be able to assign to an immutable var later.
                if is_imm {
                    let ty_str = self.currtkn.ty.to_string();
                    return Err(self.error(ParseErrTy::ImmDecl(ty_str)));
                }

                if is_class_type {
                    return Err(self.error(ParseErrTy::InvalidClassConstr));
                }

                self.consume();

                // For a var declaration without an assignment, we require a type annotation.
                // The inferrer isn't smart enough (yet) to infer types without this information
                // in all cases (ie. we don't detect if there are no future uses of this var)
                if var_ty_tkn.is_none() {
                    return Err(self.error(ParseErrTy::TyRequired));
                }

                let ty_rec = TyRecord::new(var_ty_tkn.clone().unwrap(), self.next_sym());
                let sym = Sym::new(
                    SymTy::Var,
                    is_imm,
                    ty_rec.clone(),
                    ident_tkn.clone().unwrap(),
                    None,
                    None,
                );

                let name = &ident_tkn.clone().unwrap().get_name();
                self.symtab.store(name, sym);
                let tkn = ident_tkn.clone().unwrap();

                Ok(Ast::VarDeclExpr {
                    meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
                    ty_rec: ty_rec,
                    ident_tkn: ident_tkn.unwrap(),
                    is_imm: is_imm,
                    is_global: self.symtab.is_global(),
                })
            }
            TknTy::LeftBrace => {
                // If we have a class type, retrieve the class declaration from the
                // symbol table and set the assign value of the var to that class declaration.
                // We also create a type record for that class type.
                if is_class_type {
                    self.consume();

                    // Retrieve the class specification from the symbol table so we
                    // can ensure the class is being created correctly.
                    let class_sym = self
                        .symtab
                        .retrieve(&var_ty_tkn.clone().unwrap().get_name())
                        .unwrap();

                    let expected_props = match class_sym.assign_val.clone().unwrap() {
                        Ast::ClassDeclStmt {
                            meta: _,
                            ty_rec: _,
                            ident_tkn: _,
                            methods: _,
                            props: _,
                            prop_pos,
                            ..
                        } => prop_pos,
                        _ => panic!("Class assigned to a non class ast"),
                    };
                    let mut class_props = HashMap::new();

                    loop {
                        match self.currtkn.ty.clone() {
                            TknTy::Ident(name) => {
                                // If the identifier is not a real prop for this class,
                                // we should error.
                                if !expected_props.contains_key(&name) {
                                    return Err(self.error(ParseErrTy::InvalidClassProp));
                                }

                                self.consume();
                                self.expect(TknTy::Eq)?;

                                let prop_assign = self.expr(pctx)?;
                                class_props.insert(name.clone(), prop_assign);

                                self.expect(TknTy::Comma)?;
                            }
                            _ => break,
                        }
                    }

                    self.expect(TknTy::RightBrace)?;
                    self.expect(TknTy::Semicolon)?;

                    let cl_ty_rec = TyRecord::new(var_ty_tkn.clone().unwrap(), self.next_sym());
                    let tkn = ident_tkn.clone().unwrap();

                    let constr = Ast::ClassConstrExpr {
                        meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
                        ty_rec: cl_ty_rec.clone(),
                        class_name: var_ty_tkn.clone().unwrap().get_name(),
                        props: class_props,
                    };

                    let cl_sym = Sym::new(
                        SymTy::Var,
                        is_imm,
                        cl_ty_rec.clone(),
                        ident_tkn.clone().unwrap(),
                        Some(constr.clone()),
                        None,
                    );
                    self.symtab.store(&tkn.get_name(), cl_sym);

                    Ok(Ast::VarAssignExpr {
                        meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
                        ty_rec: cl_ty_rec,
                        ident_tkn: ident_tkn.clone().unwrap(),
                        is_imm: is_imm,
                        is_global: self.symtab.is_global(),
                        value: Box::new(constr),
                    })
                } else {
                    // If we find a left brace but we're not actually trying to create a
                    // class, we should return an error.
                    let ty_str = self.currtkn.ty.to_string();
                    Err(self.error(ParseErrTy::InvalidAssign(ty_str)))
                }
            }
            _ => {
                let ty_str = self.currtkn.ty.to_string();
                Err(self.error(ParseErrTy::InvalidAssign(ty_str)))
            }
        }
    }

    /// Parses a function declaration.
    fn fn_decl(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        self.expect(TknTy::Fn)?;
        let fn_ident_tkn = self.currtkn.clone();
        self.consume();

        let mut params = Vec::new();
        self.expect(TknTy::LeftParen)?;

        while self.currtkn.ty != TknTy::RightParen {
            if params.len() > FN_PARAM_MAX_LEN {
                return Err(self.error(ParseErrTy::FnParamCntExceeded(FN_PARAM_MAX_LEN)));
            }

            let ident_tkn = self.currtkn.clone();
            self.consume();
            self.expect(TknTy::Tilde)?;

            let mut ty_rec = TyRecord::new(self.currtkn.clone(), self.next_sym());
            ty_rec.tkn = ident_tkn.clone();

            // We must create an assign value if the parameter is a class. This is because
            // when parsing the function body, we might need to access the class props/methods
            // and we can't do that unless we store the class declaration there.
            let assign_val = match ty_rec.ty.clone() {
                KolgaTy::Class(name) => {
                    let class_sym = self.symtab.retrieve(&name);
                    if class_sym.is_none() {
                        return Err(self.error(ParseErrTy::UndeclaredSym(name)));
                    }

                    class_sym.unwrap().assign_val.clone()
                }
                _ => None,
            };

            params.push(ty_rec.clone());

            // Store param variable name in the symbol table for the function scope.
            let param_sym = Sym::new(
                SymTy::Param,
                false,
                ty_rec,
                ident_tkn.clone(),
                assign_val,
                None,
            );
            self.symtab.store(&ident_tkn.get_name(), param_sym);

            self.consume();
            if self.currtkn.ty == TknTy::RightParen {
                break;
            }
            self.expect(TknTy::Comma)?;
        }

        self.expect(TknTy::RightParen)?;
        self.expect(TknTy::Tilde)?;

        let fn_ret_ty_tkn = match self.currtkn.is_ty() {
            true => {
                let tkn = self.currtkn.clone();
                self.consume();
                Some(tkn)
            }
            false => None,
        };

        if fn_ret_ty_tkn.is_none() {
            let ty_str = self.currtkn.ty.to_string();
            return Err(self.error(ParseErrTy::InvalidTy(ty_str)));
        }

        // Create and store the function sym before we parse the body and
        // set an actual value. This is so that when parsing the body, if we
        // encounter a recursive call, we won't report an error for trying
        // to call an undefined function.
        let fn_ty_rec = TyRecord::new(fn_ret_ty_tkn.clone().unwrap(), self.next_sym());
        let fn_sym = Sym::new(
            SymTy::Fn,
            true,
            fn_ty_rec.clone(),
            fn_ident_tkn.clone(),
            None,
            Some(params.clone()),
        );

        let name = &fn_ident_tkn.get_name();
        self.symtab.store(name, fn_sym);

        // Now we parse the function body, update the symbol and store it with
        // the updated body.
        let fn_body = self.block_stmt(pctx)?;
        let new_sym = Sym::new(
            SymTy::Fn,
            true,
            fn_ty_rec.clone(),
            fn_ident_tkn.clone(),
            Some(fn_body.clone()),
            Some(params.clone()),
        );

        self.symtab.store(name, new_sym);

        Ok(Ast::FnDeclStmt {
            meta: MetaAst::new(self.next(), fn_ident_tkn.line, fn_ident_tkn.pos),
            ident_tkn: fn_ident_tkn,
            fn_params: params,
            ret_ty: fn_ty_rec,
            fn_body: Box::new(fn_body),
            sc: self.symtab.finalized_level,
        })
    }

    /// Parses a class declaration
    fn class_decl(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        self.expect(TknTy::Class)?;
        let class_tkn = self.currtkn.clone();
        self.consume();
        self.expect(TknTy::LeftBrace)?;

        // Initialize a new scope for the class methods + props
        self.symtab.init_sc();
        let mut methods = Vec::new();
        let mut props = Vec::new();
        let mut prop_map = HashMap::new();
        let mut class_ctx = ClassContext::new(HashMap::new(), Vec::new());

        let mut prop_ctr = 0;
        loop {
            match self.currtkn.ty {
                TknTy::Let => {
                    let prop_ast = self.var_decl(pctx)?;
                    match prop_ast.clone() {
                        Ast::VarDeclExpr {
                            meta: _,
                            ty_rec: _,
                            ident_tkn,
                            ..
                        } => {
                            prop_map.insert(ident_tkn.get_name(), prop_ctr);
                            class_ctx.prop_map.insert(ident_tkn.get_name(), prop_ctr);
                        }
                        Ast::VarAssignExpr { .. } => {
                            return Err(self.error(ParseErrTy::ClassPropAssign));
                        }
                        _ => {
                            return Err(self.error(ParseErrTy::InvalidClassProp));
                        }
                    }
                    props.push(prop_ast);
                    prop_ctr = prop_ctr + 1;
                }
                TknTy::Fn => {
                    let result = self.fn_decl(pctx)?;
                    class_ctx.methods.push(result.clone());
                    methods.push(result);
                }
                TknTy::RightBrace => {
                    self.consume();
                    break;
                }
                _ => {
                    let ty_str = self.currtkn.ty.to_string();
                    self.error(ParseErrTy::InvalidTkn(ty_str));
                    break;
                }
            }
        }

        let final_sc_lvl = self.symtab.finalize_sc();
        let cl_ty_rec = TyRecord::new(class_tkn.clone(), self.next_sym());
        let ast = Ast::ClassDeclStmt {
            meta: MetaAst::new(self.next(), class_tkn.line, class_tkn.pos),
            ty_rec: cl_ty_rec.clone(),
            ident_tkn: class_tkn.clone(),
            methods: methods,
            props: props,
            prop_pos: prop_map,
            sc: final_sc_lvl,
        };

        // This should be stored in the starting level of the symbol table, not the
        // scope opened to store the class methods/props (which is why we close the
        // current scope before this call to store()).
        let sym = Sym::new(
            SymTy::Class,
            true,
            cl_ty_rec,
            class_tkn.clone(),
            Some(ast.clone()),
            None,
        );
        self.symtab.store(&class_tkn.get_name(), sym);

        Ok(ast)
    }

    /// Parses a statement. This function does not perform any scope management, which
    /// is delegated to each statement type.
    fn stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        match self.currtkn.ty {
            TknTy::If => self.if_stmt(pctx),
            TknTy::While => self.while_stmt(pctx),
            TknTy::For => self.for_stmt(pctx),
            TknTy::Return => self.ret_stmt(pctx),
            TknTy::LeftBrace => self.block_stmt(pctx),
            _ => self.expr_stmt(pctx),
        }
    }

    /// Parses a block statement, beginning with a '{' token. This creates a new scope,
    /// parses any statements within the block, and closes the block scope at the end.
    fn block_stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        self.expect(TknTy::LeftBrace)?;
        let mut stmts = Vec::new();
        self.symtab.init_sc();

        loop {
            match self.currtkn.ty {
                TknTy::RightBrace | TknTy::Eof => break,
                _ => {
                    let result = self.decl(pctx)?;
                    stmts.push(result);
                }
            };
        }

        self.expect(TknTy::RightBrace)?;
        let sc_lvl = self.symtab.finalize_sc();

        Ok(Ast::BlckStmt {
            meta: MetaAst::new(self.next(), self.currtkn.line, self.currtkn.pos),
            stmts: stmts,
            sc: sc_lvl,
        })
    }

    /// Parse an if statement, including else and elif blocks. These are stored in the
    /// IfStmt Ast type.
    fn if_stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let ast_line = self.currtkn.line;
        let ast_pos = self.currtkn.pos;
        self.expect(TknTy::If)?;

        let if_cond = self.expr(pctx)?;
        let if_blck = self.block_stmt(pctx)?;
        let mut else_blck = Vec::new();
        let mut else_ifs = Vec::new();
        let mut else_cnt = 0;

        loop {
            match self.currtkn.ty {
                TknTy::Elif => {
                    self.consume();
                    let elif_ast = self.expr(pctx)?;
                    let elif_blck = self.block_stmt(pctx)?;
                    let stmt_ast = Ast::ElifStmt {
                        meta: MetaAst::new(self.next(), self.currtkn.line, self.currtkn.pos),
                        cond_expr: Box::new(elif_ast),
                        stmts: Box::new(elif_blck),
                    };
                    else_ifs.push(stmt_ast);
                }
                TknTy::Else => {
                    else_cnt = else_cnt + 1;
                    self.consume();
                    let blck = self.block_stmt(pctx)?;
                    else_blck.push(blck);
                }
                _ => break,
            };
        }

        if else_cnt > 1 {
            self.error(ParseErrTy::InvalidIfStmt);
        }

        Ok(Ast::IfStmt {
            meta: MetaAst::new(self.next(), ast_line, ast_pos),
            cond_expr: Box::new(if_cond),
            if_stmts: Box::new(if_blck),
            elif_exprs: else_ifs,
            el_stmts: else_blck,
        })
    }

    fn while_stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let ast_line = self.currtkn.line;
        let ast_pos = self.currtkn.pos;
        self.expect(TknTy::While)?;

        // TODO: skip expr for infinite loop when we have a break stmt
        let while_cond = self.expr(pctx)?;
        let while_stmts = self.block_stmt(pctx)?;

        Ok(Ast::WhileStmt {
            meta: MetaAst::new(self.next(), ast_line, ast_pos),
            cond_expr: Box::new(while_cond),
            stmts: Box::new(while_stmts),
        })
    }

    fn for_stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let ast_line = self.currtkn.line;
        let ast_pos = self.currtkn.pos;
        self.expect(TknTy::For)?;

        let mut for_var_decl = None;
        let mut for_var_cond = None;
        let mut for_incr_expr = None;

        match self.currtkn.ty {
            TknTy::Semicolon => self.consume(),
            TknTy::Let => {
                let var = self.var_decl(pctx)?;
                for_var_decl = Some(var);
            }
            _ => {
                return Err(self.error(ParseErrTy::InvalidForStmt));
            }
        };

        match self.currtkn.ty {
            TknTy::Semicolon => self.consume(),
            _ => {
                let expr = self.expr_stmt(pctx)?;
                for_var_cond = Some(expr);
            }
        };

        match self.currtkn.ty {
            TknTy::Semicolon => self.consume(),
            _ => {
                let expr = self.expr_stmt(pctx)?;
                for_incr_expr = Some(expr);
            }
        };

        let for_stmt = self.block_stmt(pctx)?;

        Ok(Ast::ForStmt {
            meta: MetaAst::new(self.next(), ast_line, ast_pos),
            for_var_decl: Box::new(for_var_decl.unwrap()),
            for_cond_expr: Box::new(for_var_cond.unwrap()),
            for_step_expr: Box::new(for_incr_expr.unwrap()),
            stmts: Box::new(for_stmt),
        })
    }

    fn ret_stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let ast_line = self.currtkn.line;
        let ast_pos = self.currtkn.pos;
        self.expect(TknTy::Return)?;

        match self.currtkn.ty {
            TknTy::Semicolon => {
                self.consume();
                Ok(Ast::RetStmt {
                    meta: MetaAst::new(self.next(), ast_line, ast_pos),
                    ret_expr: None,
                })
            }
            _ => {
                let ret_expr = self.expr(pctx)?;
                self.expect(TknTy::Semicolon)?;
                Ok(Ast::RetStmt {
                    meta: MetaAst::new(self.next(), ast_line, ast_pos),
                    ret_expr: Some(Box::new(ret_expr)),
                })
            }
        }
    }

    fn expr_stmt(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let ast_line = self.currtkn.line;
        let ast_pos = self.currtkn.pos;

        let expr = self.expr(pctx)?;
        self.expect(TknTy::Semicolon)?;

        Ok(Ast::ExprStmt {
            meta: MetaAst::new(self.next(), ast_line, ast_pos),
            expr: Box::new(expr),
        })
    }

    fn expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        self.assign_expr(pctx)
    }

    fn assign_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let ast = self.logicor_expr(pctx)?;

        match self.currtkn.ty {
            TknTy::Eq => {
                let op = self.currtkn.clone();
                self.consume();
                let rhs = self.assign_expr(pctx)?;

                match ast.clone() {
                    Ast::PrimaryExpr { meta: _, ty_rec } => {
                        match ty_rec.tkn.ty {
                            TknTy::Ident(name) => {
                                let maybe_sym = self.symtab.retrieve(&name);
                                if maybe_sym.is_none() {
                                    return Err(self.error(ParseErrTy::UndeclaredSym(name)));
                                }

                                let sym = maybe_sym.unwrap();
                                if sym.imm {
                                    return Err(self.error(ParseErrTy::InvalidImmAssign(name)));
                                }

                                return Ok(Ast::VarAssignExpr {
                                    meta: MetaAst::new(
                                        self.next(),
                                        sym.ident_tkn.line,
                                        sym.ident_tkn.pos,
                                    ),
                                    ty_rec: sym.ty_rec.clone(),
                                    ident_tkn: sym.ident_tkn.clone(),
                                    is_imm: sym.imm,
                                    is_global: self.symtab.is_global(),
                                    value: Box::new(rhs),
                                });
                            }
                            _ => {
                                return Err(self.error(ParseErrTy::InvalidAssign(
                                    ty_rec.tkn.ty.clone().to_string(),
                                )));
                            }
                        };
                    }
                    Ast::ClassPropAccessExpr {
                        meta: _,
                        ty_rec,
                        ident_tkn,
                        prop_name,
                        idx,
                        owner_class,
                    } => {
                        return Ok(Ast::ClassPropSetExpr {
                            meta: MetaAst::new(self.next(), ident_tkn.line, ident_tkn.pos),
                            ty_rec: ty_rec.clone(),
                            ident_tkn: ident_tkn,
                            prop_name: prop_name,
                            idx: idx,
                            owner_class: owner_class,
                            assign_val: Box::new(rhs),
                        });
                    }
                    _ => {
                        return Err(self.error_w_pos(
                            op.line,
                            op.pos,
                            ParseErrTy::InvalidAssign(op.ty.to_string()),
                        ));
                    }
                }
            }
            _ => (),
        };

        Ok(ast)
    }

    fn logicor_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.logicand_expr(pctx)?;

        loop {
            match self.currtkn.ty {
                TknTy::PipePipe | TknTy::Or => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.logicand_expr(pctx)?;
                    ast = Ast::LogicalExpr {
                        meta: MetaAst::new(self.next(), op.line, op.pos),
                        ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                        op_tkn: op,
                        lhs: Box::new(ast),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(ast)
    }

    fn logicand_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.eq_expr(pctx)?;

        loop {
            match self.currtkn.ty {
                TknTy::AmpAmp | TknTy::And => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.eq_expr(pctx)?;
                    ast = Ast::LogicalExpr {
                        meta: MetaAst::new(self.next(), op.line, op.pos),
                        ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                        op_tkn: op,
                        lhs: Box::new(ast),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(ast)
    }

    fn eq_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.cmp_expr(pctx)?;

        loop {
            match self.currtkn.ty {
                TknTy::BangEq | TknTy::EqEq => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.cmp_expr(pctx)?;
                    ast = Ast::BinaryExpr {
                        meta: MetaAst::new(self.next(), op.line, op.pos),
                        ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                        op_tkn: op,
                        lhs: Box::new(ast),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(ast)
    }

    fn cmp_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.addsub_expr(pctx)?;

        loop {
            match self.currtkn.ty {
                TknTy::Lt | TknTy::LtEq | TknTy::Gt | TknTy::GtEq => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.addsub_expr(pctx)?;
                    ast = Ast::BinaryExpr {
                        meta: MetaAst::new(self.next(), op.line, op.pos),
                        ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                        op_tkn: op,
                        lhs: Box::new(ast),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(ast)
    }

    fn addsub_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.muldiv_expr(pctx)?;
        loop {
            match self.currtkn.ty {
                TknTy::Plus | TknTy::Minus => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.muldiv_expr(pctx)?;
                    ast = Ast::BinaryExpr {
                        meta: MetaAst::new(self.next(), op.line, op.pos),
                        ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                        op_tkn: op,
                        lhs: Box::new(ast),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(ast)
    }

    fn muldiv_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.unary_expr(pctx)?;
        loop {
            match self.currtkn.ty {
                TknTy::Star | TknTy::Slash => {
                    let op = self.currtkn.clone();
                    self.consume();
                    let rhs = self.unary_expr(pctx)?;
                    ast = Ast::BinaryExpr {
                        meta: MetaAst::new(self.next(), op.line, op.pos),
                        ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                        op_tkn: op,
                        lhs: Box::new(ast),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(ast)
    }

    fn unary_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        match self.currtkn.ty {
            TknTy::Bang | TknTy::Minus => {
                let op = self.currtkn.clone();
                self.consume();
                let rhs = self.unary_expr(pctx)?;

                return Ok(Ast::UnaryExpr {
                    meta: MetaAst::new(self.next(), op.line, op.pos),
                    ty_rec: TyRecord::unknown(op.clone(), self.next_sym()),
                    op_tkn: op,
                    rhs: Box::new(rhs),
                });
            }
            _ => self.fncall_expr(pctx),
        }
    }

    fn fncall_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        let mut ast = self.primary_expr(pctx)?;
        let ident_tkn = match ast.clone() {
            Ast::PrimaryExpr { meta: _, ty_rec } => Some(ty_rec.tkn),
            _ => None,
        };

        // If this is a class ident, we expect a period and then either a property name
        // or a function call. If this is a regular function ident, we expect an
        // opening paren next.
        match self.currtkn.ty {
            TknTy::LeftParen => {
                ast = self.fnparams_expr(pctx, ident_tkn, None)?;
            }
            TknTy::Period => {
                ast = self.class_expr(pctx, ident_tkn)?;
            }
            _ => (),
        };

        Ok(ast)
    }

    /// Parses calling class methods or getting/setting class props.
    fn class_expr(
        &mut self,
        pctx: &ParseContext,
        class_tkn: Option<Token>,
    ) -> Result<Ast, ParseErr> {
        // Consume period token
        self.expect(TknTy::Period)?;

        // This token can be a function name, a class prop name, or
        // another class name.
        let name_tkn = self.match_ident_tkn();
        let ast = match self.currtkn.ty {
            TknTy::LeftParen => {
                // Calling a function that belongs to the class
                // TODO: can we do less cloning and unwrapping here?
                let class_sym = self.symtab.retrieve(&class_tkn.clone().unwrap().get_name());
                let class_name = match class_sym.clone().unwrap().assign_val.clone().unwrap() {
                    Ast::ClassConstrExpr {
                        meta: _,
                        ty_rec: _,
                        class_name,
                        ..
                    } => class_name.clone(),
                    _ => {
                        self.error(ParseErrTy::UndeclaredSym(
                            name_tkn.clone().unwrap().get_name(),
                        ));
                        String::new()
                    }
                };

                let fn_ast = self.fnparams_expr(pctx, name_tkn.clone(), class_sym.clone())?;
                let params = fn_ast.extract_params();
                let tkn = class_tkn.clone().unwrap();

                Ok(Ast::ClassFnCallExpr {
                    meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
                    ty_rec: fn_ast.get_ty_rec().unwrap(),
                    class_tkn: tkn.clone(),
                    class_name: class_name,
                    fn_tkn: name_tkn.unwrap().clone(),
                    fn_params: params,
                    sc: self.symtab.level(),
                })
            }
            TknTy::Period => {
                // Accessing another class within this class
                self.class_expr(pctx, name_tkn)
            }
            _ => {
                // A typical class property access
                // Get the class construction expression from the symbol table. We use this
                // to get class metadata
                let class_sym = self.symtab.retrieve(&class_tkn.clone().unwrap().get_name());
                if class_sym.is_none() {
                    return Err(self.error(ParseErrTy::UndeclaredSym(
                        name_tkn.clone().unwrap().get_name(),
                    )));
                }
                let class_ptr = class_sym.unwrap();
                let owner = class_ptr.assign_val.clone().unwrap();

                let class_name = match &owner {
                    Ast::ClassConstrExpr {
                        meta: _,
                        ty_rec: _,
                        class_name,
                        ..
                    } => Some(class_name.clone()),
                    _ => None,
                };

                if class_name.is_none() {
                    return Err(self.error(ParseErrTy::InvalidClassProp));
                }

                // We also need the class declaration in addition to the class construction
                // above. This is needed to get the property position within the prop
                // array in the class. Doing this here will make it slightly easier to
                // generate LLVM IR (because we need the index for GEP instructions).
                let class_decl_sym = self.symtab.retrieve(&class_name.clone().unwrap());
                if class_decl_sym.is_none() {
                    return Err(self.error(ParseErrTy::UndeclaredSym(class_name.unwrap())));
                }

                // Get the prop position from the declaration.
                let pos = match class_decl_sym.unwrap().assign_val.clone().unwrap() {
                    Ast::ClassDeclStmt {
                        meta: _,
                        ty_rec: _,
                        ident_tkn: _,
                        methods: _,
                        props: _,
                        prop_pos,
                        ..
                    } => {
                        let map = prop_pos.clone();
                        let idx = map.get(&name_tkn.clone().unwrap().get_name());
                        match idx {
                            Some(num) => num.clone() as usize,
                            None => {
                                self.error(ParseErrTy::InvalidClassProp);
                                0 as usize
                            }
                        }
                    }
                    _ => 0 as usize,
                };

                // Get the type record from the class construction.
                let prop_ty_rec = match &owner {
                    Ast::ClassConstrExpr {
                        meta: _,
                        ty_rec: _,
                        class_name: _,
                        props,
                        ..
                    } => {
                        let mb_prop = props.get(&name_tkn.clone().unwrap().get_name());
                        if mb_prop.is_none() {
                            return Err(self.error(ParseErrTy::InvalidClassProp));
                        }

                        mb_prop.unwrap().get_ty_rec()
                    }
                    _ => None,
                };

                if prop_ty_rec.is_none() {
                    return Err(self.error(ParseErrTy::InvalidClassProp));
                }

                let tkn = class_tkn.clone().unwrap();

                Ok(Ast::ClassPropAccessExpr {
                    meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
                    ty_rec: prop_ty_rec.unwrap(),
                    ident_tkn: class_tkn.unwrap(),
                    prop_name: name_tkn.unwrap().get_name(),
                    idx: pos,
                    owner_class: Box::new(owner),
                })
            }
        };

        ast
    }

    /// Parses the parameters of a function call. Because the function could be a class method,
    /// this accepts an optional class symbol, which should be taken out of the symbol table. If this
    /// is not a class method being parsed, maybe_class_sym should be None.
    /// This symbol is used to find the expected function params, so that we can ensure that
    /// what is passed in is correct.
    fn fnparams_expr(
        &mut self,
        pctx: &ParseContext,
        fn_tkn: Option<Token>,
        maybe_class_sym: Option<Rc<Sym>>,
    ) -> Result<Ast, ParseErr> {
        self.expect(TknTy::LeftParen)?;

        let fn_sym = self.symtab.retrieve(&fn_tkn.clone().unwrap().get_name());
        let mut fn_ty_rec = match fn_sym {
            Some(ref sym) => Some(sym.ty_rec.clone()),
            None => None,
        };

        // If the fn_sym doesn't exist, we need to handle the case that it might be
        // a class method, so we check the class symbol if one exists.
        let maybe_expected_params = match fn_sym {
            // If there is no class sym and no fn sym, we have no expected params.
            None if maybe_class_sym.is_none() => None,
            // If there is a class sym, check for the method in the class methods list
            // and get the expected params. If the method doesn't exist on the class,
            // we return None.
            None => {
                let class_constr_ast = maybe_class_sym.unwrap().assign_val.clone().unwrap();
                let class_name = match class_constr_ast {
                    Ast::ClassConstrExpr {
                        meta: _,
                        ty_rec: _,
                        class_name,
                        ..
                    } => Some(class_name.clone()),
                    _ => None,
                };

                if class_name.is_none() {
                    return Err(self.error(ParseErrTy::UndeclaredSym(
                        fn_tkn.clone().unwrap().get_name(),
                    )));
                }

                let class_decl_sym = self.symtab.retrieve(&class_name.unwrap()).unwrap();
                let params = match class_decl_sym.assign_val.clone().unwrap() {
                    Ast::ClassDeclStmt {
                        meta: _,
                        ty_rec: _,
                        ident_tkn: _,
                        methods,
                        ..
                    } => {
                        let mut expected_params = None;
                        for mtod_ast in methods {
                            match mtod_ast {
                                Ast::FnDeclStmt {
                                    meta: _,
                                    ident_tkn,
                                    fn_params,
                                    ret_ty,
                                    ..
                                } => {
                                    if ident_tkn.get_name() == fn_tkn.clone().unwrap().get_name() {
                                        expected_params = Some(fn_params);
                                        fn_ty_rec = Some(ret_ty.clone());
                                    }
                                }
                                _ => (),
                            }
                        }

                        expected_params
                    }
                    _ => None,
                };

                params
            }
            // If the fn sym exists, simply take its params.
            Some(ref sym) => sym.fn_params.clone(),
        };

        // If we have no expected params after checking the fn_sym and the class_sym,
        // we report an error and return None early.
        if maybe_expected_params.is_none() {
            let tkn = fn_tkn.clone().unwrap();
            return Err(self.error_w_pos(
                tkn.line,
                tkn.pos,
                ParseErrTy::UndeclaredSym(tkn.get_name()),
            ));
        }

        let expected_params = maybe_expected_params.unwrap();
        let mut params: Vec<Ast> = Vec::new();
        while self.currtkn.ty != TknTy::RightParen {
            if params.len() > FN_PARAM_MAX_LEN {
                return Err(self.error(ParseErrTy::FnParamCntExceeded(FN_PARAM_MAX_LEN)));
            }

            let parm = self.expr(pctx)?;
            params.push(parm);

            if self.currtkn.ty == TknTy::RightParen {
                break;
            }
            self.expect(TknTy::Comma)?;
        }

        self.expect(TknTy::RightParen)?;

        if expected_params.len() != params.len() {
            let tkn = fn_tkn.clone().unwrap();
            self.error_w_pos(
                tkn.line,
                tkn.pos,
                ParseErrTy::WrongFnParamCnt(expected_params.len(), params.len()),
            );
        }

        if fn_ty_rec.is_none() {
            let tkn = fn_tkn.clone().unwrap();
            return Err(self.error_w_pos(
                tkn.line,
                tkn.pos,
                ParseErrTy::UndeclaredSym(tkn.get_name()),
            ));
        }

        let tkn = fn_tkn.clone().unwrap();

        Ok(Ast::FnCallExpr {
            meta: MetaAst::new(self.next(), tkn.line, tkn.pos),
            ty_rec: fn_ty_rec.unwrap(),
            fn_tkn: fn_tkn.unwrap(),
            fn_params: params,
        })
    }

    fn primary_expr(&mut self, pctx: &ParseContext) -> Result<Ast, ParseErr> {
        match self.currtkn.ty.clone() {
            TknTy::Str(_) | TknTy::Val(_) | TknTy::True | TknTy::False | TknTy::Null => {
                let ast = Ok(Ast::PrimaryExpr {
                    meta: MetaAst::new(self.next(), self.currtkn.line, self.currtkn.pos),
                    ty_rec: TyRecord::new(self.currtkn.clone(), self.next_sym()),
                });
                self.consume();
                ast
            }
            TknTy::Ident(ref ident_name) => {
                let mb_sym = self.symtab.retrieve(ident_name);
                if mb_sym.is_none() {
                    let err = self.error(ParseErrTy::UndeclaredSym(ident_name.to_string()));
                    self.consume();
                    return Err(err);
                }

                let sym = mb_sym.unwrap();
                // If we have no assign value, but we are looking at a param
                // or function decl sym, we can return the sym. But no assign value on
                // any other type requires a check that we are assigning to it, otherwise
                // we are trying to access an undefined variable.
                // Fn is here to support recursive calls.
                if sym.assign_val.is_none()
                    && (sym.sym_ty != SymTy::Param && sym.sym_ty != SymTy::Fn)
                {
                    let next_tkn = self.lexer.peek_tkn();
                    // If the following token is '=', we don't need to report an error
                    // for unitialized var (we are initializing it here).
                    if next_tkn.ty != TknTy::Eq {
                        // TODO: how to handle 'self' in var references here? we need to know
                        // which class self refers to, so we can look up the decl and check if
                        // the following var that appears after the period does exist.

                        let err = self.error(ParseErrTy::UnassignedVar(ident_name.to_string()));
                        self.consume();
                        return Err(err);
                    }
                }

                let mut ty_rec = sym.ty_rec.clone();
                ty_rec.tkn = self.currtkn.clone();
                let ast = Ok(Ast::PrimaryExpr {
                    meta: MetaAst::new(self.next(), self.currtkn.line, self.currtkn.pos),
                    ty_rec: ty_rec.clone(),
                });
                self.consume();
                ast
            }
            TknTy::LeftParen => {
                self.consume();
                let ast = self.expr(pctx)?;
                self.expect(TknTy::RightParen)?;
                Ok(ast)
            }
            TknTy::String | TknTy::Num | TknTy::Bool => {
                let ty_str = self.currtkn.ty.to_string();
                let err = self.error(ParseErrTy::InvalidAssign(ty_str));
                self.consume();
                Err(err)
            }
            _ => {
                let ty_str = self.currtkn.ty.to_string();
                let err = self.error(ParseErrTy::InvalidTkn(ty_str));
                self.consume();
                Err(err)
            }
        }
    }

    fn match_ident_tkn(&mut self) -> Option<Token> {
        match self.currtkn.ty {
            TknTy::Ident(_) => {
                let tkn = Some(self.currtkn.clone());
                self.consume();
                tkn
            }
            _ => {
                let ty_str = self.currtkn.ty.to_string();
                self.error(ParseErrTy::InvalidIdent(ty_str));
                None
            }
        }
    }

    /// Check that the current token is the same as the one we expect. If it is, consume the
    /// token and advance. If it isn't report an error.
    fn expect(&mut self, tknty: TknTy) -> Result<(), ParseErr> {
        if self.currtkn.ty == tknty {
            self.consume();
            Ok(())
        } else {
            let ty_str = self.currtkn.ty.to_string();
            let err_ty = ParseErrTy::TknMismatch(tknty.to_string(), ty_str);
            Err(ParseErr::new(self.currtkn.line, self.currtkn.pos, err_ty))
        }
    }

    /// Advance to the next token, discarded the previously read token.
    fn consume(&mut self) {
        self.currtkn = self.lexer.lex();
    }

    /// Report a parsing error from the current token, with the given parser error type.
    fn error(&mut self, ty: ParseErrTy) -> ParseErr {
        let err = ParseErr::new(self.currtkn.line, self.currtkn.pos, ty);
        self.errors.push(err.clone());
        err
    }

    /// Report a parsing error at a given location with a provided error type.
    fn error_w_pos(&mut self, line: usize, pos: usize, ty: ParseErrTy) -> ParseErr {
        let err = ParseErr::new(line, pos, ty);
        self.errors.push(err.clone());
        err
    }

    /// Increments the node count and returns the number of the next node in the AST.
    fn next(&mut self) -> usize {
        self.node_count = self.node_count + 1;
        self.node_count
    }

    fn next_sym(&mut self) -> usize {
        self.sym_count = self.sym_count + 1;
        self.sym_count
    }
}
