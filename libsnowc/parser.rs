use tyobj::TyObj;
use lexer::Lexer;
use errors::report::Report;

pub struct ParserResult {
    instrs: Vec<TyObj>,
    error: Vec<Report>
}

pub struct Parser<'l> {
    lexer: &'l Lexer
}

impl<'l> Parser<'l> {
    pub fn new(lex: &mut Lexer) -> Parser {
        Parser {
            lexer: lex
        }
    }

    pub fn parse(&self) -> ParserResult {
        unimplemented!()
    }
}
