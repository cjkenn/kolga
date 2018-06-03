use std::rc::Rc;

use tyobj::{TyObj, Expr};
use lexer::Lexer;
use token::{Token, TokenTy};
use errors::report::ErrReport;

pub struct ParserResult {
    objs: Vec<TyObj>,
    error: Vec<ErrReport>
}

impl ParserResult {
    pub fn new() -> ParserResult {
        ParserResult {
            objs: Vec::new(),
            error: Vec::new()
        }
    }
}

pub struct Parser<'l> {
    lexer: &'l mut Lexer,
    pub result: ParserResult,
    currtkn: Rc<Option<Token>>
}

impl<'l> Parser<'l> {
    pub fn new(lex: &mut Lexer) -> Parser {
        let firsttkn = Rc::new(lex.lex());
        Parser {
            lexer: lex,
            result: ParserResult::new(),
            currtkn: firsttkn
        }
    }

    pub fn parse(&mut self) {
        while *self.currtkn != None {
            match self.parse_decl() {
                Some(obj) => self.result.objs.push(obj),
                None => ()
            }
        }
    }

    fn parse_decl(&mut self) -> Option<TyObj> {
        match *Rc::clone(&self.currtkn) {
            Some(ref tkn) if tkn.ty == TokenTy::Let => self.parse_var_decl(),
            Some(ref tkn) if tkn.ty == TokenTy::Class => self.parse_class_decl(),
            Some(ref tkn) if tkn.ty == TokenTy::Func => self.parse_func_decl(),
            Some(_) => self.parse_stmt(),
            None => {
                panic!("snow: Should not have encountered a token with value 'None' when parsing decl.")
            }
        }
    }

    fn parse_var_decl(&mut self) -> Option<TyObj> {
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

        Some(TyObj::Var(var_ty, ident, is_imm, var_expr))
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
