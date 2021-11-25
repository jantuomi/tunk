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

const REPL_HISTORY_FILENAME: &str = "repl_history.txt";

fn main() -> Result<(), String> {
    use ast::Program;
    use pest::Parser;
    use std::env;
    use std::fs;

    let mut args = env::args();
    let command = args.next().unwrap();
    let subcommand_result = args.next();
    if subcommand_result.is_none() {
        println!("usage: {} run FILE", command.as_str());
        println!("       {} repl", command.as_str());
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
            use rustyline::error::ReadlineError;
            use rustyline::Editor;
            use std::collections::HashMap;
            use std::rc::Rc;

            println!("Tunk REPL");
            println!("\nCommands:");
            println!("#exit         exit REPL");
            println!("#symbols      display symbol table\n");

            let mut rl = Editor::<()>::new();
            if rl.load_history(REPL_HISTORY_FILENAME).is_err() {
                println!(
                    "[repl] no history file found, will store REPL history in {}",
                    REPL_HISTORY_FILENAME
                );
            }

            let mut symbol_table: HashMap<String, Rc<Term>> = HashMap::new();
            let mut line: String = "".to_owned();
            let mut is_appending = false;
            loop {
                let readline = if is_appending {
                    line.push('\n');
                    rl.readline("| ")
                } else {
                    line.clear();
                    rl.readline("> ")
                };

                match readline {
                    Ok(raw_line) => {
                        line.push_str(raw_line.trim());

                        if line.starts_with("#exit") {
                            rl.add_history_entry(line.as_str());
                            rl.save_history(REPL_HISTORY_FILENAME).unwrap();
                            break;
                        } else if line.starts_with("#symbols") {
                            if symbol_table.is_empty() {
                                println!("[repl] symbol table is empty");
                            }
                            for (symbol, expr) in &symbol_table {
                                println!("{}:\n{}\n", symbol, *expr);
                            }

                            rl.add_history_entry(line.as_str());
                            rl.save_history(REPL_HISTORY_FILENAME).unwrap();
                            continue;
                        } else if !line.ends_with(';') && !line.is_empty() && !line.starts_with('#')
                        {
                            is_appending = true;
                            continue;
                        } else {
                            is_appending = false;
                        }

                        rl.add_history_entry(line.as_str());
                        // Regular execution
                        let parse_tree_result =
                            lparser::LParser::parse(lparser::Rule::program, &line);
                        if let Err(err) = parse_tree_result {
                            println!("{}\n", err);

                            rl.save_history(REPL_HISTORY_FILENAME).unwrap();
                            continue;
                        }

                        let mut parse_tree = parse_tree_result.unwrap();
                        let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);

                        let process_result =
                            runtime::process(&syntax_tree, Some(&mut symbol_table));
                        if let Err(err) = process_result {
                            println!("{}\n", err);
                        }

                        rl.save_history(REPL_HISTORY_FILENAME).unwrap();
                    }
                    Err(ReadlineError::Interrupted) => {
                        println!("CTRL-C");
                        break;
                    }
                    Err(ReadlineError::Eof) => {
                        println!("CTRL-D");
                        break;
                    }
                    Err(err) => {
                        println!("{:?}", err);
                        break;
                    }
                };
            }

            Ok(())
        }
        _ => Err("invalid subcommand".to_owned()),
    }
}
