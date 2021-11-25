#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest;

mod ast;
mod builtins;
mod lparser;
mod runtime;

#[cfg(test)]
mod test;

#[macro_export]
macro_rules! extract_enum_value {
    ($value:expr, $pattern:pat => $extracted_value:expr) => {
        match $value {
            $pattern => $extracted_value,
            _ => panic!("extract_enum_value: Pattern doesn't match!"),
        }
    };
}

fn main() -> Result<(), String> {
    use ast::Program;
    use pest::Parser;
    use std::env;
    use std::fs;

    let mut args = env::args();
    let command = args.next().unwrap();
    let subcommand_result = args.next();
    if subcommand_result.is_none() {
        println!("usage: {c} run FILE\n       {c} repl", c = command.as_str());
        return Ok(());
    }
    let subcommand = subcommand_result.unwrap();

    match subcommand.as_str() {
        "run" => {
            let script_path = args.next().ok_or("[run] no script file specified")?;
            let source = fs::read_to_string(script_path).or(Err("[run] cannot read file"))?;
            let parse_tree_result = lparser::LParser::parse(lparser::Rule::program, &source);
            if parse_tree_result.is_err() {
                println!("{}", parse_tree_result.unwrap_err());
                return Err("[run] failed to parse".to_owned());
            }
            let mut parse_tree = parse_tree_result.unwrap();
            let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);
            let process_result = runtime::process(&syntax_tree, None);
            if process_result.is_err() {
                println!("{}", process_result.unwrap_err());
                return Err("[run] failed to run".to_owned());
            }
            Ok(())
        }
        "repl" => {
            use runtime::*;
            use std::collections::HashMap;
            use std::io::{stdin, stdout, Write};
            use std::rc::Rc;

            println!("Tunk REPL");
            println!("\"#exit\" to exit");
            println!("\"#symbols\" to display symbol table\n");

            let mut symbol_table: HashMap<String, Rc<Term>> = HashMap::new();
            let mut s = String::new();
            let mut appending_input = false;
            loop {
                if appending_input {
                    s.push('\n');
                    print!("| ");
                } else {
                    s.clear();
                    print!("> ");
                }
                let _ = stdout().flush();
                let read_line_result = stdin().read_line(&mut s);
                if read_line_result.is_err() {
                    println!("{}\n", read_line_result.unwrap_err());
                    continue;
                }
                s = s.trim().to_owned();
                if s.starts_with("#exit") {
                    return Ok(());
                } else if s.starts_with("#symbols") {
                    if symbol_table.is_empty() {
                        println!("[repl] symbol table is empty");
                    }
                    for (symbol, expr) in &symbol_table {
                        println!("{}:\n{}\n", symbol, *expr);
                    }
                    continue;
                } else if !s.ends_with(';') && !s.is_empty() && !s.starts_with('#') {
                    appending_input = true;
                    continue;
                } else {
                    appending_input = false;
                }

                let parse_tree_result = lparser::LParser::parse(lparser::Rule::program, &s);
                if parse_tree_result.is_err() {
                    println!("{}\n", parse_tree_result.unwrap_err());
                    continue;
                }
                let mut parse_tree = parse_tree_result.unwrap();
                let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);
                let process_result = runtime::process(&syntax_tree, Some(&mut symbol_table));
                if process_result.is_err() {
                    println!("{}\n", process_result.unwrap_err());
                    continue;
                }
            }
        }
        _ => Err("invalid subcommand".to_owned()),
    }
}
