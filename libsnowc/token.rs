#[derive(Debug, PartialEq, Clone)]
pub enum TknTy {
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

    // Identifiers/literals
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

impl TknTy {
    // TODO: this isnt a token type, but a language type. Need a different data
    // structure to hold this
    pub fn to_equiv_ty(&self) -> TknTy {
        match self {
            TknTy::Str(_) => TknTy::String,
            TknTy::Val(_) => TknTy::Num,
            TknTy::True | TknTy::False => TknTy::Bool,
            TknTy::Minus => TknTy::Num,
            TknTy::Bang => TknTy::Bool,
            _ => self.clone()
        }
    }

    pub fn is_numerical(&self) -> bool {
        match self {
            TknTy::Num | TknTy::Val(_) => true,
            _ => false
        }
    }

    pub fn is_bin_op(&self) -> bool {
        match self {
            TknTy::Plus |
            TknTy::Minus |
            TknTy::Star |
            TknTy::Slash |
            TknTy::Percent |
            TknTy::EqEq |
            TknTy::BangEq |
            TknTy::Gt |
            TknTy::Lt |
            TknTy::GtEq |
            TknTy::LtEq => true,
            _ => false
        }
    }

    pub fn is_numerical_op(&self) -> bool {
        match self {
            TknTy::Plus |
            TknTy::Minus |
            TknTy::Star |
            TknTy::Slash |
            TknTy::Percent => true,
            _ => false
        }
    }

    pub fn is_cmp_op(&self) -> bool {
        match self {
            TknTy::EqEq |
            TknTy::BangEq |
            TknTy::Gt |
            TknTy::Lt |
            TknTy::GtEq |
            TknTy::LtEq => true,
            _ => false
        }
    }

    pub fn is_logical_op(&self) -> bool {
        match self {
            TknTy::AmpAmp | TknTy::PipePipe | TknTy::Or | TknTy::And => true,
             _ => false
        }
    }

    pub fn is_unary_op(&self) -> bool {
        match self {
            TknTy::Minus | TknTy::Bang => true,
            _ => false
        }

    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ty: TknTy,
    pub line: usize,
    pub pos: usize
}

impl Token {
    pub fn new(ty: TknTy, line: usize, pos: usize) -> Token {
        Token {
            ty: ty,
            line: line,
            pos: pos
        }
    }

    pub fn is_ty(&self) -> bool {
        self.ty == TknTy::Num ||
            self.ty == TknTy::String ||
            self.ty == TknTy::Bool
    }
}
