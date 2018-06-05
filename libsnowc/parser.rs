use std::rc::Rc;

use ast::Ast;
use symtab::SymTab;
use lexer::Lexer;
use token::{Token, TokenTy};
use errors::report::ErrC;

pub struct ParserResult {
    ast: Option<Ast>,
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
    currtkn: Rc<Option<Token>>
}

impl<'l, 's> Parser<'l, 's> {
    pub fn new(lex: &'l mut Lexer, symt: &'s mut SymTab) -> Parser<'l, 's> {
        let firsttkn = Rc::new(lex.lex());

        Parser {
            lexer: lex,
            symtab: symt,
            errors: Vec::new(),
            currtkn: firsttkn
        }
    }

    pub fn parse(&mut self) -> ParserResult {
        let mut stmts: Vec<Ast> = Vec::new();

        while *self.currtkn != None {
            match self.parse_decl() {
                Some(a) => stmts.push(a),
                None => ()
            }
        }

        let head = Ast::Prog(stmts);
        ParserResult {
            ast: Some(head),
            error: self.errors.clone()
        }
    }

    fn parse_decl(&mut self) -> Option<Ast> {
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
    fn parse_var_decl(&mut self) -> Option<Ast> {
        self.expect(TokenTy::Let);

        // let is_imm = match *Rc::clone(&self.currtkn) {
        //     Some(ref tkn) if tkn.ty == TokenTy::Imm => {
        //         self.consume();
        //         true
        //     },
        //     _ => false
        // };

        // let var_ty = match *Rc::clone(&self.currtkn) {
        //     Some(ref tkn) if tkn.is_ty() => {
        //         self.consume();
        //         Some(tkn.clone())
        //     },
        //     Some(ref tkn) => {
        //         self.err_from_tkn(tkn, format!("{:?} is not a valid type", tkn.ty));
        //         None
        //     },
        //     _ => None
        // };

        // let ident = match *Rc::clone(&self.currtkn) {
        //     Some(ref tkn) => match tkn.ty.clone() {
        //         TokenTy::Ident(name) => {
        //             self.consume();
        //             Some(name)
        //         },
        //         _ => {
        //             self.err_from_tkn(tkn, format!("{:?} token is not a valid identifier", tkn.ty));
        //             None
        //         }
        //     },
        //     _ => None
        // };

        None
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
        let error = ErrC::new(tkn.line, tkn.pos, message);
        self.errors.push(error);
    }

    fn err_wo_tkn(&mut self, message: String) {
        // TODO: this is bad error messaging, need to do something with line/pos
        let error = ErrC::new(0, 0, message);
        self.errors.push(error);
    }
}
