use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "ir/ir.pest"] // relative to src
pub struct IRParser;
