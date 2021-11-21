#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest;

mod ast;
mod lparser;
mod runtime;

fn main() {
    use ast::Program;
    use pest::Parser;
    use std::env;
    use std::fs;

    let script_path = env::args().nth(1).expect("no script file specified");

    let unparsed_file = fs::read_to_string(script_path).expect("cannot read file");
    let parse_tree_result = lparser::LParser::parse(lparser::Rule::program, &unparsed_file);

    if parse_tree_result.is_err() {
        println!("{}", parse_tree_result.unwrap_err());
        return;
    }

    let mut parse_tree = parse_tree_result.unwrap();

    // println!("parse tree = {:#?}", parse_tree);
    let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);
    // println!("syntax tree = {:#?}", syntax_tree);

    runtime::evaluate(&syntax_tree);
}
