use std::fmt;

#[derive(PartialEq, Clone)]
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
    Tilde,

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

impl fmt::Debug for TknTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pretty_ty = match self {
            TknTy::LeftParen => "(".to_string(),
            TknTy::RightParen => ")".to_string(),
            TknTy::LeftBrace => "{".to_string(),
            TknTy::RightBrace => "}".to_string(),
            TknTy::LeftBracket => "[".to_string(),
            TknTy::RightBracket => "]".to_string(),
            TknTy::Semicolon => ";".to_string(),
            TknTy::Eq => "=".to_string(),
            TknTy::Lt => "<".to_string(),
            TknTy::Gt => ">".to_string(),
            TknTy::Period => ".".to_string(),
            TknTy::Comma => ",".to_string(),
            TknTy::Bang => "!".to_string(),
            TknTy::Plus => "+".to_string(),
            TknTy::Minus => "-".to_string(),
            TknTy::Star => "*".to_string(),
            TknTy::Slash => "/".to_string(),
            TknTy::Percent => "%".to_string(),
            TknTy::Amp => "&".to_string(),
            TknTy::Pipe => "|".to_string(),
            TknTy::Tilde => "~".to_string(),
            TknTy::EqEq => "==".to_string(),
            TknTy::LtEq => "<=".to_string(),
            TknTy::GtEq => ">=".to_string(),
            TknTy::BangEq => "!=".to_string(),
            TknTy::AmpAmp => "&&".to_string(),
            TknTy::PipePipe => "||".to_string(),
            TknTy::Ident(name) => name.to_string(),
            TknTy::Str(name) => name.to_string(),
            TknTy::Val(val) => val.to_string(),
            TknTy::Let => "let".to_string(),
            TknTy::Imm => "imm".to_string(),
            TknTy::Func => "func".to_string(),
            TknTy::Return => "return".to_string(),
            TknTy::Class => "class".to_string(),
            TknTy::This => "this".to_string(),
            TknTy::If => "if".to_string(),
            TknTy::Elif => "elif".to_string(),
            TknTy::Then => "then".to_string(),
            TknTy::Else => "else".to_string(),
            TknTy::While => "while".to_string(),
            TknTy::In => "in".to_string(),
            TknTy::For => "for".to_string(),
            TknTy::Num => "num".to_string(),
            TknTy::String => "string".to_string(),
            TknTy::Bool => "bool".to_string(),
            TknTy::True => "true".to_string(),
            TknTy::False => "false".to_string(),
            TknTy::Or => "or".to_string(),
            TknTy::And => "and".to_string(),
            TknTy::Null => "null".to_string(),
            TknTy::Eof => "EOF".to_string()
        };

        write!(f, "{}", pretty_ty)
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

    pub fn get_name(&self) -> String {
        match self.ty {
            TknTy::Ident(ref name) => name.to_string(),
            _ => "".to_string()
        }
    }

    pub fn is_ident(&self) -> bool {
        match self.ty {
            TknTy::Ident(_) => true,
            _ => false
        }
    }
}
