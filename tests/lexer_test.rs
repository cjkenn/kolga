extern crate kolgac;

use std::fs::File;

use kolgac::lexer::Lexer;
use kolgac::token::TknTy;

#[test]
fn lexer() {
    let file = File::open("./tests/lexer/lex").unwrap();
    let mut lexer = Lexer::new(file);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::LeftParen);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::RightParen);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::LeftBrace);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::RightBrace);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::LeftBracket);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::RightBracket);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Semicolon);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Period);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Comma);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Plus);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Minus);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Star);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Percent);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Tilde);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Slash);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eq);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::EqEq);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Lt);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::LtEq);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Gt);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::GtEq);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Bang);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::BangEq);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Amp);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::AmpAmp);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Pipe);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::PipePipe);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Val(100.0));

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Ident(String::from("ident")));

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Str(String::from("string")));

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Let);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Imm);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Fn);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Return);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Class);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::This);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::If);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Elif);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Then);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Else);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Else);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::While);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::In);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::For);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Num);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::String);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::String);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Bool);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::True);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::False);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Or);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::And);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Null);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Void);

    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);

    println!("PASS: lexing all symbols and keywords");
}
