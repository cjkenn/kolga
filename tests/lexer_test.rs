extern crate syntax;

use std::fs::File;

use syntax::lexer::Lexer;
use syntax::token::TokenTy;

#[test]
fn test_lex_empty() {
    let file = File::open("./tests/lexer_input/empty").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn, None);
}

#[test]
fn test_lex_unrecognized() {
    let file = File::open("./tests/lexer_input/unrecognized").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn, None);
}

#[test]
fn test_lex_string_lit() {
    let file = File::open("./tests/lexer_input/stringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex().unwrap();
    assert_eq!(tkn.ty, TokenTy::Str(String::from("teststr")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_integer_lit() {
    let file = File::open("./tests/lexer_input/integerlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex().unwrap();
    assert_eq!(tkn.ty, TokenTy::Val(10.0));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_float_lit() {
    let file = File::open("./tests/lexer_input/floatlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex().unwrap();
    assert_eq!(tkn.ty, TokenTy::Val(10.55));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_unterminated_str_lit() {
    let file = File::open("./tests/lexer_input/untermstringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex();
    assert_eq!(tkn, None);
}

#[test]
fn test_lex_ident() {
    let file = File::open("./tests/lexer_input/ident").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex().unwrap();
    assert_eq!(tkn.ty, TokenTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);
}

#[test]
fn test_lex_ident_string_lit() {
    let file = File::open("./tests/lexer_input/identstringlit").unwrap();
    let mut lexer = Lexer::new(file);
    let tkn = lexer.lex().unwrap();
    assert_eq!(tkn.ty, TokenTy::Ident(String::from("ident")));
    assert_eq!(tkn.line, 1);
    assert_eq!(tkn.pos, 0);

    let tkn2 = lexer.lex().unwrap();
    assert_eq!(tkn2.ty, TokenTy::Str(String::from("teststr")));
    assert_eq!(tkn2.line, 1);
    assert_eq!(tkn2.pos, 6);
}
