extern crate kolgac;

use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct ParseExpect {
    is_pass: bool,
    line: usize,
    pos: usize,
    error: String
}

#[test]
fn parser() {
    // TODO: get the input directory right
    let inputs = fs::read_dir("./tests/parser_input").ok().unwrap();

    for entry in inputs {
        let path = entry.unwrap().path();
        let file = File::open(path).unwrap();

        // Parse test expectations in file comment
        let expectations = BufReader::new(file).lines().next().unwrap();
        let expected = parse_expectations(expectations.unwrap());

        // Run the parser and match the parse result with expectations
    }
}

fn parse_expectations(expectations: String) -> ParseExpect {
    let parts: Vec<&str> = expectations.split("::").collect();
    let pass = parts[1] == "pass";

    ParseExpect {
        is_pass: pass,
        line: 0,
        pos: 0,
        error: String::new()
    }
}
