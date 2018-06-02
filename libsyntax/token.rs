#[derive(Debug, PartialEq, Clone)]
pub enum TokenTy {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Semicolon,
    Eq,
    Lt,
    Gt,
    Period,
    Comma,
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Pipe,

    // Multi character tokens
    EqEq,
    LtEq,
    GtEq,
    BangEq,
    AmpAmp,
    PipePipe,

    // Values
    Ident(String),
    Str(String),
    Val(f64),

    // Keywords
    Let,
    Imm,
    Func,
    Return,
    Class,
    This,
    If,
    Elif,
    Then,
    Else,
    While,
    In,
    For,
    Num,
    String,
    Bool,
    True,
    False,
    Or,
    And,
    Null,

    Eof
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ty: TokenTy,
    pub line: usize,
    pub pos: usize
}

impl Token {
    pub fn new(ty: TokenTy, line: usize, pos: usize) -> Token {
        Token {
            ty: ty,
            line: line,
            pos: pos
        }
    }
}
