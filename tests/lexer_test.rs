extern crate kolgac;

use std::fs::File;

use kolgac::lexer::Lexer;
use kolgac::token::TknTy;

#[test]
fn lex_empty() {
    let file = File::open("./tests/lexer_input/empty").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn lex_unrecognized() {
    let file = File::open("./tests/lexer_input/unrecognized").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn lex_string_lit() {
    let file = File::open("./tests/lexer_input/stringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Str(String::from("teststr")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn lex_integer_lit() {
    let file = File::open("./tests/lexer_input/integerlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Val(10.0));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn lex_float_lit() {
    let file = File::open("./tests/lexer_input/floatlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Val(10.55));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn lex_unterminated_str_lit() {
    let file = File::open("./tests/lexer_input/untermstringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn lex_ident() {
    let file = File::open("./tests/lexer_input/ident").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn lex_ident_string_lit() {
    let file = File::open("./tests/lexer_input/identstringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);

    let tkn2 = lexer.lex();
    assert_eq!(tkn2.ty, TknTy::Str(String::from("teststr")));
    assert_eq!(tkn2.line, 1);
    assert_eq!(tkn2.pos, 6);
}

#[test]
fn lex_comment() {
    let file = File::open("./tests/lexer_input/comment").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn lex_comment_w_line() {
    let file = File::open("./tests/lexer_input/commentandident").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 2);
    assert_eq!(tkn.pos, 0);

}
