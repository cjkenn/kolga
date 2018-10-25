use std::io::{BufReader, BufRead, Seek, SeekFrom};
use std::fs::File;
use std::collections::HashMap;

use error::lex::{LexErr, LexErrTy};
use error::KolgaErr;
use token::{Token, TknTy};

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
    reserved: HashMap<String, TknTy>,

    /// Number of cumulative bytes read by the reader in this lexer
    bytes_read: usize
}

impl Lexer {
    pub fn new(infile: File) -> Lexer {
        let mut reader = BufReader::new(infile);
        let mut buf = String::new();
        let init_bytes = reader.read_line(&mut buf).expect("reading from buffer won't fail");

        let buffer: Vec<char> = buf.chars().collect();
        let currch = if buffer.len() == 0 {
            None
        } else {
            Some(buffer[0])
        };

        let r: HashMap<String, TknTy> =
            [
                (String::from("let"), TknTy::Let),
                (String::from("imm"), TknTy::Imm),
                (String::from("fn"), TknTy::Fn),
                (String::from("return"), TknTy::Return),
                (String::from("class"), TknTy::Class),
                (String::from("this"), TknTy::This),
                (String::from("if"), TknTy::If),
                (String::from("elif"), TknTy::Elif),
                (String::from("then"), TknTy::Then),
                (String::from("el"), TknTy::Else),
                (String::from("else"), TknTy::Else),
                (String::from("while"), TknTy::While),
                (String::from("in"), TknTy::In),
                (String::from("for"), TknTy::For),
                (String::from("num"), TknTy::Num),
                (String::from("string"), TknTy::String),
                (String::from("str"), TknTy::String),
                (String::from("bool"), TknTy::Bool),
                (String::from("true"), TknTy::True),
                (String::from("false"), TknTy::False),
                (String::from("or"), TknTy::Or),
                (String::from("and"), TknTy::And),
                (String::from("null"), TknTy::Null),
                (String::from("void"), TknTy::Void)
            ].iter().cloned().collect();

        Lexer {
            curr: currch,
            linenum: 1,
            pos: 0,
            reader: reader,
            buffer: buffer,
            reserved: r,
            bytes_read: init_bytes
        }
    }

    /// Get the next token from the input stream. If this returns None, it means we're either
    /// at the end of the input, or we've encountered a character we don't recognize.
    pub fn lex(&mut self) -> Token {
        if self.curr.is_none() {
            return self.eof_tkn();
        }

        // Skip whitespace
        while self.curr.unwrap().is_whitespace() {
            self.advance();
            if self.curr.is_none() {
                return self.eof_tkn();
            }
        }

        let ch = self.curr.unwrap();
        match ch {
            '(' => self.consume(TknTy::LeftParen),
            ')' => self.consume(TknTy::RightParen),
            '{' => self.consume(TknTy::LeftBrace),
            '}' => self.consume(TknTy::RightBrace),
            '[' => self.consume(TknTy::LeftBracket),
            ']' => self.consume(TknTy::RightBracket),
            ';' => self.consume(TknTy::Semicolon),
            '.' => self.consume(TknTy::Period),
            ',' => self.consume(TknTy::Comma),
            '+' => self.consume(TknTy::Plus),
            '-' => self.consume(TknTy::Minus),
            '*' => self.consume(TknTy::Star),
            '%' => self.consume(TknTy::Percent),
            '~' => self.consume(TknTy::Tilde),
            '"' => self.lex_str(),
            '/' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '/' => {
                        while self.curr.unwrap() != '\n' {
                            self.advance();
                        }
                        return self.lex();
                    }
                    _ => self.consume(TknTy::Slash)
                }
            },
            '=' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TknTy::EqEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TknTy::Eq)
                }
            },
            '<' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TknTy::LtEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TknTy::Lt)
                }
            },
            '>' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TknTy::GtEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TknTy::Gt)
                }
            },
            '!' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '=' => {
                        let tkn = self.consume(TknTy::BangEq);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TknTy::Bang)
                }
            },
            '&' => {
                let nextch = self.peek();
                 match nextch {
                    Some(ch) if ch == '&' => {
                        let tkn = self.consume(TknTy::AmpAmp);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TknTy::Amp)
                 }
            },
            '|' => {
                let nextch = self.peek();
                match nextch {
                    Some(ch) if ch == '|' => {
                        let tkn = self.consume(TknTy::PipePipe);
                        self.advance();
                        tkn
                    }
                    _ => self.consume(TknTy::Pipe)
                 }
            },
            _ if ch.is_digit(10) => self.lex_num(),
            _ if ch.is_alphabetic() => self.lex_ident(),
            _ => {
                LexErr::new(self.linenum, self.pos, LexErrTy::UnknownChar(ch)).emit();
                self.eof_tkn()
            }
        }
    }

    /// Look ahead to the next token, and then reest the buffer and rewind
    /// the reader for future calls to lex().
    pub fn peek_tkn(&mut self) -> Token {
        // Copy the current state of the lexer
        let start_curr = self.curr;
        let start_pos = self.pos;
        let start_line = self.linenum;
        let start_buffer = self.buffer.clone();
        let start_bytes_read = self.bytes_read;

        let tkn = self.lex();

        // Rewind the reader to its place before we performed
        // reads in the lex() call
        let seek_bytes = (self.bytes_read - start_bytes_read) as i64;
        self.reader.seek(SeekFrom::Current(-seek_bytes));

        // Reset the state of the lexer to what it was before the peek
        self.curr = start_curr;
        self.pos = start_pos;
        self.linenum = start_line;
        self.buffer = start_buffer;

        tkn
    }

    /// Lex a string literal. We expect to have a " character when this
    /// function is called, and we consume the last " character during
    /// this call.
    fn lex_str(&mut self) -> Token {
        let mut lit = String::new();
        let startpos = self.pos;
        let startline = self.linenum;

        // Consume '"'
        self.advance();

        while !self.finished() {
            match self.curr {
                Some(ch) => {
                    if ch == '"' {
                        return self.consume_w_pos(TknTy::Str(lit), startline, startpos);
                    } else {
                        lit.push(ch);
                        self.advance();
                    }
                },
                None => {
                    LexErr::new(self.linenum, self.pos, LexErrTy::UnterminatedStr(lit)).emit();
                    return self.eof_tkn();
                }
            }
        }

        LexErr::new(self.linenum, self.pos, LexErrTy::UnterminatedStr(lit)).emit();
        self.eof_tkn()
    }

    /// Lex a floating point or integer literal.
    fn lex_num(&mut self) -> Token {
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
        Token::new(TknTy::Val(numval), startline, startpos)
    }

    /// Lex an identifier. This is not a string literal and does not
    /// contain quotations around it.
    fn lex_ident(&mut self) -> Token {
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

        let mut ty = TknTy::Ident(lit.clone());

        if self.reserved.contains_key(&lit) {
            ty = self.reserved.get(&lit).unwrap().clone();
        }

        Token::new(ty, startline, startpos)
    }

    /// Consume current char and return a token from it.
    fn consume(&mut self, ty: TknTy) -> Token {
        let tkn = Token::new(ty, self.linenum, self.pos);
        self.advance();
        tkn
    }

    /// Consume current char and return a token from it, given a line
    /// and char position. Used so that the correct line/pos combo can be reported
    /// for identifiers, literals, and numbers.
    fn consume_w_pos(&mut self, ty: TknTy, line: usize, pos: usize) -> Token {
        let tkn = Token::new(ty, line, pos);
        self.advance();
        tkn
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

        if self.pos == self.buffer.len()-1 || on_new_line {
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
        let line_bytes = self.reader.read_line(&mut buf).expect("file reader should not fail");
        self.buffer = buf.chars().collect();
        self.bytes_read = self.bytes_read + line_bytes;

        self.pos = 0;
        self.linenum = self.linenum + 1;
    }

    /// When the input buffer is empty, that means read_line has indicated
    /// we're at the end of the file.
    fn finished(&self) -> bool {
        self.buffer.len() == 0
    }

    fn eof_tkn(&self) -> Token {
        Token::new(TknTy::Eof, self.linenum, self.pos)
    }
}
