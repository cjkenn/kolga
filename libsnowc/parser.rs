use std::rc::Rc;

use tyobj::{TyObj, Expr};
use op::OpCode;
use symtab::SymTab;
use lexer::Lexer;
use token::{Token, TokenTy};
use errors::report::ErrReport;

pub struct ParserResult {
    ops: Vec<OpCode>,
    error: Vec<ErrReport>
}

impl ParserResult {
    pub fn new() -> ParserResult {
        ParserResult {
            ops: Vec::new(),
            error: Vec::new()
        }
    }
}

pub struct Parser<'l, 's> {
    lexer: &'l mut Lexer,
    symtab: &'s mut SymTab,
    result: ParserResult,
    currtkn: Rc<Option<Token>>
}

impl<'l, 's> Parser<'l, 's> {
    pub fn new(lex: &'l mut Lexer, symt: &'s mut SymTab) -> Parser<'l, 's> {
        let firsttkn = Rc::new(lex.lex());

        Parser {
            lexer: lex,
            symtab: symt,
            result: ParserResult::new(),
            currtkn: firsttkn
        }
    }

    pub fn parse(&mut self) {
        while *self.currtkn != None {
            self.parse_decl();
        }
    }

    fn parse_decl(&mut self) -> Vec<OpCode> {
        match *Rc::clone(&self.currtkn) {
            Some(ref tkn) if tkn.ty == TokenTy::Let => self.parse_var_decl(),
            // Some(ref tkn) if tkn.ty == TokenTy::Class => self.parse_class_decl(),
            // Some(ref tkn) if tkn.ty == TokenTy::Func => self.parse_func_decl(),
            // Some(_) => self.parse_stmt(),
            _ => {
                panic!("snow: Should not have encountered a token with value 'None' when parsing decl.")
            }
        }
    }

    /// Takes an input of "let" ["imm"] type ident "=" expr ";"
    /// Returns a vector of operations storing the name and type of this
    /// variable into the vm environment.
    /// Ex.
    /// let x num = 10;
    /// produces
    /// ST "x", 10
    ///
    /// let x num = 2 + 2;
    /// produces
    /// MV r1, 2
    /// MV r2, 2
    /// ADD r3, r1, r2
    /// ST "x", r3
    fn parse_var_decl(&mut self) -> Vec<OpCode> {
        self.expect(TokenTy::Let);

        let is_imm = match *Rc::clone(&self.currtkn) {
            Some(ref tkn) if tkn.ty == TokenTy::Imm => {
                self.consume();
                true
            },
            _ => false
        };

        let var_ty = match *Rc::clone(&self.currtkn) {
            Some(ref tkn) if tkn.is_ty() => {
                self.consume();
                Some(tkn.clone())
            },
            Some(ref tkn) => {
                self.err_from_tkn(tkn, format!("{:?} is not a valid type", tkn.ty));
                None
            },
            _ => None
        };

        let ident = match *Rc::clone(&self.currtkn) {
            Some(ref tkn) => match tkn.ty.clone() {
                TokenTy::Ident(_) => {
                    self.consume();
                    Some(tkn.clone())
                },
                _ => {
                    self.err_from_tkn(tkn, format!("{:?} token is not a valid identifier", tkn.ty));
                    None
                }
            },
            _ => None
        };

        self.expect(TokenTy::Eq);
        let var_expr = self.parse_expr();
        self.expect(TokenTy::Semicolon);
        Vec::new()
    }

    fn parse_func_decl(&mut self) -> Option<TyObj> {
        unimplemented!()
    }

    fn parse_class_decl(&mut self) -> Option<TyObj> {
        unimplemented!()
    }

    fn parse_stmt(&mut self) -> Option<TyObj> {
        unimplemented!()
    }

    fn parse_expr(&mut self) -> Expr {
        unimplemented!()
    }

    /// Check that the current token is the same as the one we expect. If it is, consume the
    /// token and advance. If it isn't report an error.
    fn expect(&mut self, tknty: TokenTy) {
        match *Rc::clone(&self.currtkn) {
            Some(ref tkn) if tkn.ty == tknty => self.consume(),
            Some(ref tkn) => {
                self.err_from_tkn(tkn, format!("Expected token '{:?}', but found '{:?}'", tknty, tkn.ty))
            },
            None => self.err_wo_tkn(format!("Expected token '{:?}', but found 'None'", tknty))
        }
    }

    fn consume(&mut self) {
        self.currtkn = Rc::new(self.lexer.lex());
    }

    fn err_from_tkn(&mut self, tkn: &Token, message: String) {
        let error = ErrReport::new(tkn.line, tkn.pos, message);
        self.result.error.push(error);
    }

    fn err_wo_tkn(&mut self, message: String) {
        // TODO: this is bad error messaging, need to do something with line/pos
        let error = ErrReport::new(0, 0, message);
        self.result.error.push(error);
    }
}
