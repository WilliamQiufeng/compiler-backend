use std::{fs::File, io::Read, path::PathBuf};

use pest::Parser;

use super::parser::{IRParser, Rule};

#[test]
fn test() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/ir/test.ir");
    println!("{}", path.display());
    let mut test_file = File::open(path.to_str().expect("Path conversion err")).expect("File err");
    let mut buf = String::new();
    test_file.read_to_string(&mut buf).expect("Cannot read");
    let parse = IRParser::parse(Rule::program, buf.as_str());
    println!("{:?}", parse);
    match parse {
        Ok(r) => {
            
        },
        Err(e) => {
            println!("{}", e);
        },
    }
}