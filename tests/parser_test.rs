extern crate kolgac;

use kolgac::lexer::Lexer;
use kolgac::parser::Parser;
use kolgac::symtab::SymbolTable;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

struct ParseExpect {
    pub is_pass: bool,
    pub line: usize,
    pub pos: usize,
}

#[test]
fn parser() {
    let inputs = fs::read_dir("./tests/parser").ok().unwrap();

    for entry in inputs {
        let path = entry.unwrap().path();
        let file = File::open(path.clone()).unwrap();

        let expectations = BufReader::new(file).lines().next().unwrap();
        let parse_result = parse_expectations(expectations.unwrap());
        match parse_result {
            Err(e) => {
                println!("Error parsing test expectations: {:?}", e);
                return;
            }
            Ok(expectation) => {
                run_parser_test(path, expectation);
            }
        }
    }
}

fn run_parser_test(path: PathBuf, expct: ParseExpect) {
    let mut symtab = SymbolTable::new();
    let file = File::open(path.clone()).unwrap();
    let mut lexer = Lexer::new(file);
    let mut parser = Parser::new(&mut lexer, &mut symtab);

    let parse_result = parser.parse();

    match expct.is_pass {
        true => {
            if parse_result.has_err {
                assert!(
                    false,
                    "FAIL: {:?} expected successful parse, found error",
                    path.file_stem().unwrap(),
                );
            } else {
                println!("PASS: parse {:?}", path.file_stem().unwrap());
            }
        }
        false => {
            if !parse_result.has_err {
                assert!(
                    false,
                    "FAIL: {:?} expected error, found none",
                    path.file_stem().unwrap()
                );
            } else {
                println!("PASS: parse {:?}", path.file_stem().unwrap());
            }
        }
    }
}

fn parse_expectations(expectations: String) -> Result<ParseExpect, &'static str> {
    let parts: Vec<&str> = expectations.split("::").collect();
    if parts.len() == 0 {
        return Err("No test expectations string found");
    }

    if parts.len() < 2 {
        return Err("Invalid test expectation string. Usage: 'expect::[pass][fail]::[line]::[pos]'");
    }

    if !parts[0].contains("expect") {
        return Err("Invalid test expectation string. Usage: 'expect::[pass][fail]::[line]::[pos]'");
    }

    if parts[1] != "fail" && parts[1] != "pass" {
        return Err("Invalid test expectation string. Usage: 'expect::[pass][fail]::[line]::[pos]'");
    }

    if parts[1] == "pass" {
        return Ok(ParseExpect {
            is_pass: true,
            line: 0,
            pos: 0,
        });
    }

    if parts.len() < 4 {
        return Err("Invalid test expectation string. Usage: 'expect::[pass][fail]::[line]::[pos]'");
    }

    let line = parts[2].parse::<usize>();
    if line.is_err() {
        return Err("Line number in expectations must be valid int");
    }

    let pos = parts[3].parse::<usize>();
    if pos.is_err() {
        return Err("Position number in expectations must be valid int");
    }

    Ok(ParseExpect {
        is_pass: false,
        line: line.unwrap(),
        pos: pos.unwrap(),
    })
}
