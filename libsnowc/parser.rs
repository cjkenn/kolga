use ast::Ast;
use symtab::SymTab;
use lexer::Lexer;
use token::{Token, TknTy};
use errors::report::ErrC;

const FN_PARAM_MAX_LEN: usize = 64;

pub struct ParserResult {
    ast: Option<Box<Ast>>,
    error: Vec<ErrC>
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
    lexer: &'l mut Lexer,
    symtab: &'s mut SymTab,
    errors: Vec<ErrC>,
    currtkn: Token
}

impl<'l, 's> Parser<'l, 's> {
    pub fn new(lex: &'l mut Lexer, symt: &'s mut SymTab) -> Parser<'l, 's> {
        let firsttkn = lex.lex();

        Parser {
            lexer: lex,
            symtab: symt,
            errors: Vec::new(),
            currtkn: firsttkn
        }
    }

    pub fn parse(&mut self) -> ParserResult {
        let mut stmts: Vec<Ast> = Vec::new();

        while self.currtkn.ty != TknTy::Eof {
            match self.parse_decl() {
                Some(a) => stmts.push(a),
                None => ()
            }
        }

        let head = Ast::Prog(stmts);
        ParserResult {
            ast: Some(Box::new(head)),
            error: self.errors.clone()
        }
    }

    fn parse_decl(&mut self) -> Option<Ast> {
        match self.currtkn.ty {
            TknTy::Let => self.parse_var_decl(),
            TknTy::Func => self.parse_func_decl(),
            TknTy::Class => self.parse_class_decl(),
            _ => self.parse_stmt()
        }
    }

    /// Takes an input of "let" ["imm"] type ident "=" expr ";"
    fn parse_var_decl(&mut self) -> Option<Ast> {
        self.expect(TknTy::Let);

        let is_imm = match self.currtkn.ty {
            TknTy::Imm => {
                self.consume();
                true
            },
            _ => false
        };

        let var_ty_tkn = if self.currtkn.is_ty() {
            let tkn = Some(self.currtkn.clone());
            self.consume();
            tkn
        } else {
            let err_msg = format!("{:?} is not a valid type", self.currtkn.ty);
            self.err_from_tkn(err_msg);
            None
        };

        let ident_tkn = self.match_ident_tkn();

        self.expect(TknTy::Eq);
        let var_val = self.parse_expr();
        self.expect(TknTy::Semicolon);

        Some(Ast::Assign(var_ty_tkn, ident_tkn, is_imm, Box::new(var_val)))
    }

    fn parse_func_decl(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_class_decl(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_stmt(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_expr(&mut self) -> Option<Ast> {
        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_logicor_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_logicand_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_eq_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_cmp_expr(&mut self) -> Option<Ast> {
            unimplemented!()
    }

    fn parse_addsub_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_muldiv_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_unr_expr(&mut self) -> Option<Ast> {
        unimplemented!()
    }

    fn parse_fncall_expr(&mut self) -> Option<Ast> {
        let func_name_tkn = self.match_ident_tkn();
        match self.currtkn.ty {
            TknTy::LeftParen => {
                self.parse_fnparams_expr(func_name_tkn)
            },
            TknTy::Period => {
                // calling a method on a class
                self.consume();
                let class_prop_tkn = self.match_ident_tkn();
                Some(Ast::ClassCall(func_name_tkn, class_prop_tkn))
            },
            _ => {
                let err_msg = format!("Unexpected token {:?} found", self.currtkn.ty);
                self.err_from_tkn(err_msg);
                None
            }
        }
    }

    /// Parses expr { ',' expr } ; (no parens are expected here)
    fn parse_fnparams_expr(&mut self, fn_tkn: Option<Token>) -> Option<Ast> {
        let mut params: Vec<Ast> = Vec::new();
        while self.currtkn.ty != TknTy::RightParen {
            if params.len() > FN_PARAM_MAX_LEN {
                let err_msg = format!("Function param count exceeded limit of {}", FN_PARAM_MAX_LEN);
                self.err_from_tkn(err_msg);
                return None;
            }

            match self.parse_expr() {
                Some(a) => params.push(a),
                None => ()
            };

            if self.currtkn.ty == TknTy::RightParen {
                break;
            }
            self.expect(TknTy::Comma);
        }

        self.expect(TknTy::RightParen);
        Some(Ast::FnCall(fn_tkn, params))
    }

    fn parse_primary_expr(&mut self) -> Option<Ast> {
        match &self.currtkn.ty {
            TknTy::Str(_) |
            TknTy::Val(_) |
            TknTy::Ident(_) |
            TknTy::True |
            TknTy::False |
            TknTy::Null => {
                let ast = Some(Ast::Primary(self.currtkn.clone()));
                self.consume();
                ast
            },
            TknTy::LeftParen => {
                self.consume();
                let ast = self.parse_expr();
                self.expect(TknTy::RightParen);
                ast
            },
            _ => {
                let err_msg = format!("Unknown token {:?} found", self.currtkn.ty);
                self.err_from_tkn(err_msg);
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
        let error = ErrC::new(self.currtkn.line, self.currtkn.pos, message);
        self.errors.push(error);
    }
}
