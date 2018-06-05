extern crate snowc;

use std::fs::File;

use snowc::lexer::Lexer;
use snowc::token::TknTy;

#[test]
fn test_lex_empty() {
    let file = File::open("./tests/lexer_input/empty").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn test_lex_unrecognized() {
    let file = File::open("./tests/lexer_input/unrecognized").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn test_lex_string_lit() {
    let file = File::open("./tests/lexer_input/stringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Str(String::from("teststr")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_integer_lit() {
    let file = File::open("./tests/lexer_input/integerlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Val(10.0));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_float_lit() {
    let file = File::open("./tests/lexer_input/floatlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Val(10.55));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_unterminated_str_lit() {
    let file = File::open("./tests/lexer_input/untermstringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn test_lex_ident() {
    let file = File::open("./tests/lexer_input/ident").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_ident_string_lit() {
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
fn test_lex_comment() {
    let file = File::open("./tests/lexer_input/comment").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Eof);
}

#[test]
fn test_lex_comment_w_line() {
    let file = File::open("./tests/lexer_input/commentandident").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn.ty, TknTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 2);
    assert_eq!(tkn.pos, 0);

}
