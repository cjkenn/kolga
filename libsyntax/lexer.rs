use std::io::{BufReader, BufRead};
use std::fs::File;
use std::collections::HashMap;

use errors::report::Report;
use token::{Token, TokenTy};

#[derive(Debug)]
pub struct Lexer {
    /// Current character in input buffer
    pub curr: Option<char>,

    /// Current line number
    pub linenum: usize,

    /// Current char position in line
    pub pos: usize,

    /// File reader
    reader: BufReader<File>,

    /// Buffer holding the current line
    buffer: Vec<char>,

    /// Reserved words mapping
    reserved: HashMap<String, TokenTy>
}

impl Lexer {
    pub fn new(infile: File) -> Lexer {
        let mut reader = BufReader::new(infile);
        let mut buf = String::new();
        reader.read_line(&mut buf).ok();

        let buffer: Vec<char> = buf.chars().collect();
        let currch = if buffer.len() == 0 {
            None
        } else {
            Some(buffer[0])
        };

        let r: HashMap<String, TokenTy> =
            [
                (String::from("let"), TokenTy::Let),
                (String::from("imm"), TokenTy::Imm),
                (String::from("func"), TokenTy::Func),
                (String::from("fnc"), TokenTy::Func),
                (String::from("return"), TokenTy::Return),
                (String::from("ret"), TokenTy::Return),
                (String::from("class"), TokenTy::Class),
                (String::from("cls"), TokenTy::Class),
                (String::from("this"), TokenTy::This),
                (String::from("if"), TokenTy::If),
                (String::from("elif"), TokenTy::Elif),
                (String::from("then"), TokenTy::Then),
                (String::from("el"), TokenTy::Else),
                (String::from("else"), TokenTy::Else),
                (String::from("while"), TokenTy::While),
                (String::from("in"), TokenTy::In),
                (String::from("for"), TokenTy::For),
                (String::from("num"), TokenTy::Num),
                (String::from("string"), TokenTy::String),
                (String::from("str"), TokenTy::String),
                (String::from("bool"), TokenTy::Bool),
                (String::from("true"), TokenTy::True),
                (String::from("false"), TokenTy::False),
                (String::from("or"), TokenTy::Or),
                (String::from("and"), TokenTy::And),
                (String::from("null"), TokenTy::Null)
            ].iter().cloned().collect();

        Lexer {
            curr: currch,
            linenum: 1,
            pos: 0,
            reader: reader,
            buffer: buffer,
            reserved: r
        }
    }

    /// Get the next token from the input stream. If this returns None, it means we're either
    /// at the end of the input, or we've encountered a character we don't recognize.
    pub fn lex(&mut self) -> Option<Token> {
        if self.curr.is_none() {
            return None;
        }

        // Skip whitespace
        while self.curr.unwrap().is_whitespace() {
            self.advance();
            if self.curr.is_none() {
                return None;
            }
        }

        let ch = self.curr.unwrap();
        match ch {
            '(' => self.consume(TokenTy::LeftParen),
            ')' => self.consume(TokenTy::RightParen),
            '{' => self.consume(TokenTy::LeftBrace),
            '}' => self.consume(TokenTy::RightBrace),
            '[' => self.consume(TokenTy::LeftBracket),
            ']' => self.consume(TokenTy::RightBracket),
            ';' => self.consume(TokenTy::Semicolon),
            '.' => self.consume(TokenTy::Period),
            ',' => self.consume(TokenTy::Comma),
            '+' => self.consume(TokenTy::Plus),
            '-' => self.consume(TokenTy::Minus),
            '*' => self.consume(TokenTy::Star),
            '/' => self.consume(TokenTy::Slash),
            '%' => self.consume(TokenTy::Percent),
            '"' => self.lex_str(),
            '=' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TokenTy::EqEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TokenTy::Eq)
                }
            },
            '<' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TokenTy::LtEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TokenTy::Lt)
                }
            },
            '>' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TokenTy::GtEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TokenTy::Gt)
                }
            },
            '!' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TokenTy::BangEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TokenTy::Bang)
                }
            },
            '&' => {
                let nextch = self.peek();
                 match nextch {
                    Some(ch) if ch == '&' => {
                        let tkn = self.consume(TokenTy::AmpAmp);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TokenTy::Amp)
                 }
            },
            '|' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '|' => {
                        let tkn = self.consume(TokenTy::PipePipe);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TokenTy::Pipe)
                 }
            },
            _ if ch.is_digit(10) => self.lex_num(),
            _ if ch.is_alphabetic() => self.lex_ident(),
            _ => {
                let err = Report::new(self.linenum, self.pos, format!("Unrecognized character {}", ch));
                err.emit();
                None
            }
        }
    }

    /// Lex a string literal. We expect to have a " character when this
    /// function is called, and we consume the last " character during
    /// this call.
    fn lex_str(&mut self) -> Option<Token> {
        let mut lit = String::new();
        let startpos = self.pos;
        let startline = self.linenum;

        // Consume '"'
        self.advance();

        while !self.finished() {
            match self.curr {
                Some(ch) => {
                    if ch == '"' {
                        return self.consume_w_pos(TokenTy::Str(lit), startline, startpos);
                    } else {
                        lit.push(ch);
                        self.advance();
                    }
                },
                None => {
                    let err = Report::new(self.linenum,
                                          self.pos,
                                          format!("Unterminated string literal {}", lit));
                    err.emit();
                    return None;
                }
            }
        }

        let err = Report::new(self.linenum,
                              self.pos,
                              format!("Unterminated string literal {}", lit));
        err.emit();
        None
    }

    /// Lex a floating point or integer literal.
    fn lex_num(&mut self) -> Option<Token> {
        let mut lit = String::new();
        let startpos = self.pos;
        let startline = self.linenum;

        let mut currch = self.curr;

        while let Some(ch) = currch {
            if ch.is_digit(10) {
                lit.push(ch);
                self.advance();
                currch = self.curr;
            } else if ch == '.' {
                lit.push(ch);
                self.advance();
                let mut innerch = self.curr;

                while let Some(ch) = innerch {
                    if ch.is_digit(10) {
                        lit.push(ch);
                        self.advance();
                        innerch = self.curr;
                    } else {
                        innerch = None;
                        currch = None;
                    }
                }
            } else {
                currch = None;
            }
        }

        let numval = lit.parse::<f64>().unwrap();
        Some(Token::new(TokenTy::Val(numval), startline, startpos))
    }

    /// Lex an identifier. This is not a string literal and does not
    /// contain quotations around it.
    fn lex_ident(&mut self) -> Option<Token> {
        let mut lit = String::new();
        let startpos = self.pos;
        let startline = self.linenum;

        let mut currch = self.curr;

        while let Some(ch) = currch {
            if ch.is_alphanumeric() {
                lit.push(ch);
                self.advance();
                currch = self.curr;
            } else {
                currch = None;
            }
        }

        let mut ty = TokenTy::Ident(lit.clone());

        if self.reserved.contains_key(&lit) {
            ty = self.reserved.get(&lit).unwrap().clone();
        }

        Some(Token::new(ty, startline, startpos))
    }

    /// Consume current char and return a token from it.
    fn consume(&mut self, ty: TokenTy) -> Option<Token> {
        let tkn = Token::new(ty, self.linenum, self.pos);
        self.advance();
        Some(tkn)
    }

    /// Consume current char and return a token from it, given a line
    /// and char position. Used so that the correct line/pos combo can be reported
    /// for identifiers, literals, and numbers.
    fn consume_w_pos(&mut self, ty: TokenTy, line: usize, pos: usize) -> Option<Token> {
        let tkn = Token::new(ty, line, pos);
        self.advance();
        Some(tkn)
    }

    /// Return the next char in the buffer, if any.
    fn peek(&mut self) -> Option<char> {
        if self.pos >= self.buffer.len()-1 {
            return None;
        }

       Some(self.buffer[self.pos+1])
    }

    /// Move the char position ahead by 1. If we are at the end of the current
    /// buffer, reads the next line of the file into the buffer and sets
    /// the position to 0.
    fn advance(&mut self) {
        let on_new_line = match self.curr {
            Some(ch) if ch == '\n' => true,
            _ => false
        };

        if self.pos == self.buffer.len() || on_new_line {
            self.next_line();
        } else {
            self.pos = self.pos + 1;
        }

        if self.finished() {
            self.curr = None;
        } else {
            self.curr = Some(self.buffer[self.pos]);
        }
    }

    /// Read the next line of the input file into the buffer.
    fn next_line(&mut self) {
        let mut buf = String::new();
        self.reader.read_line(&mut buf).ok();
        self.buffer = buf.chars().collect();

        self.pos = 0;
        self.linenum = self.linenum + 1;
    }

    /// When the input buffer is empty, that means read_line has indicated
    /// we're at the end of the file.
    fn finished(&self) -> bool {
        self.buffer.len() == 0
    }
}
