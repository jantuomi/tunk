#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest;

mod parser {
    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct Parser;
}

mod ast {
    use super::parser::Rule;

    pub fn from_parse_tree(parse_tree: &mut pest::iterators::Pairs<Rule>) -> Program {
        let root = parse_tree.next().unwrap();

        match root.as_rule() {
            Rule::program => {
                let statements_with_eoi: Vec<pest::iterators::Pair<Rule>> =
                    root.into_inner().collect();
                let statements_split = statements_with_eoi.split_last().unwrap();
                let statements = statements_split.1.to_vec();

                statements
                    .iter()
                    .map(|s| Statement::from_pair(s.clone()))
                    .collect()
            }
            _ => panic!("[ast] first Pair is not a program"),
        }
    }

    pub type Program = Vec<Statement>;

    #[derive(Debug)]
    pub enum Statement {
        Definition {
            symbol: Symbol,
            expression: Expression,
        },
        Expression(Expression),
    }

    impl Statement {
        fn from_pair(pair: pest::iterators::Pair<Rule>) -> Statement {
            let def_or_expr = pair.into_inner().next().unwrap();

            fn definition_from_pair(pair: pest::iterators::Pair<Rule>) -> Statement {
                let mut inner = pair.into_inner();
                let symbol = inner.next().unwrap().as_span().as_str();
                let expression = inner.next().unwrap();
                let expression_inners: Vec<ExpressionInner> = expression
                    .into_inner()
                    .map(ExpressionInner::from_pair)
                    .collect();

                Statement::Definition {
                    symbol: String::from(symbol),
                    expression: expression_inners,
                }
            }

            match def_or_expr.as_rule() {
                Rule::definition => definition_from_pair(def_or_expr),
                Rule::expression => {
                    let expression_inners: Vec<ExpressionInner> = def_or_expr
                        .into_inner()
                        .map(ExpressionInner::from_pair)
                        .collect();
                    Statement::Expression(expression_inners)
                }
                rule => panic!("[ast] can't make a statement from {:#?}", rule),
            }
        }
    }

    #[derive(Debug)]
    pub enum ExpressionInner {
        Symbol(Symbol),
        IntegerLiteral(IntegerLiteral),
        StringLiteral(StringLiteral),
        Expression(Expression),
    }

    impl ExpressionInner {
        fn from_pair(pair: pest::iterators::Pair<Rule>) -> ExpressionInner {
            // println!("ExpressionInner::from_pair pair: {:#?}", pair);

            match pair.as_rule() {
                Rule::symbol => ExpressionInner::Symbol(string_from_pair(pair)),
                Rule::integer_literal => ExpressionInner::IntegerLiteral(integer_from_pair(pair)),
                Rule::string_literal => {
                    let s_with_quotes = string_from_pair(pair);
                    let mut chars = s_with_quotes.chars();
                    chars.next();
                    chars.next_back();
                    let s = chars.as_str();
                    ExpressionInner::StringLiteral(String::from(s))
                }
                Rule::expression => {
                    let expression_inners: Vec<ExpressionInner> =
                        pair.into_inner().map(ExpressionInner::from_pair).collect();
                    ExpressionInner::Expression(expression_inners)
                }
                rule => panic!("[ast] can't make an expression element from {:#?}", rule),
            }
        }
    }

    pub type Expression = Vec<ExpressionInner>;

    pub type IntegerLiteral = i64;
    fn integer_from_pair(pair: pest::iterators::Pair<Rule>) -> i64 {
        let s = pair.as_span().as_str();
        s.parse().unwrap()
    }

    pub type StringLiteral = String;
    fn string_from_pair(pair: pest::iterators::Pair<Rule>) -> String {
        String::from(pair.as_span().as_str())
    }

    pub type Symbol = String;
}

mod runtime {
    use super::ast;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[derive(Debug, Clone)]
    pub enum Value {
        Integer(i64),
        String(String),
        Var(usize),
        Function(usize, Rc<Value>),
    }

    static VAR_ID_INC: AtomicUsize = AtomicUsize::new(0);

    fn try_evaluate_builtin(symbol: &ast::Symbol) -> Option<Value> {
        match symbol.as_str() {
            "int.zero" => Some(Value::Integer(0)),
            "id" => {
                let v = VAR_ID_INC.load(Ordering::Relaxed);
                VAR_ID_INC.store(v + 1, Ordering::Relaxed);
                Some(Value::Function(v, Rc::new(Value::Var(v))))
            }
            _ => None,
        }
    }

    fn try_apply_function(func_rc: Rc<Value>, arg: Rc<Value>, bound_v: Option<usize>) -> Rc<Value> {
        let func = &*func_rc;
        match func {
            Value::Function(func_v, body_rc) => {
                let v1 = bound_v.unwrap_or(*func_v);
                let body = &**body_rc;
                match body {
                    Value::Var(v2) => {
                        if v1 == *v2 {
                            arg
                        } else {
                            (*body_rc).clone()
                        }
                    }
                    Value::Function(_, _) => try_apply_function((*body_rc).clone(), arg, Some(v1)),
                    _ => (*body_rc).clone(),
                }
            }
            _ => func_rc,
        }
    }

    fn evaluate_expr(
        symbol_table: &HashMap<String, Rc<Value>>,
        expression: &ast::Expression,
    ) -> Rc<Value> {
        // println!("[runtime] evaluating expression");
        let head = &expression[0];
        let tail = &expression[1..];
        let arity = tail.len();
        match head {
            ast::ExpressionInner::IntegerLiteral(value) => match arity {
                0 => Rc::new(Value::Integer(*value)),
                _ => panic!("[runtime] cannot apply integer: {}", value),
            },
            ast::ExpressionInner::StringLiteral(value) => match arity {
                0 => Rc::new(Value::String(value.clone())),
                _ => panic!("[runtime] cannot apply string: {}", value),
            },
            ast::ExpressionInner::Symbol(value) => match arity {
                0 => {
                    let builtin_value = try_evaluate_builtin(value);
                    if builtin_value.is_some() {
                        return Rc::new(builtin_value.unwrap());
                    }

                    let table_lookup_value = symbol_table.get(value);
                    if table_lookup_value.is_some() {
                        let lookup_rc = table_lookup_value.unwrap();
                        return lookup_rc.clone();
                    }

                    Rc::new(Value::String(String::from("dummy value")))
                }
                1 => {
                    // let arg = &tail[0];
                    // let applied = try_apply_function(value, arg, None);
                    panic!("should apply function here!")
                }
                _ => unreachable!(
                    "[runtime] expression arity >= 2! there must be an error in AST generation"
                ),
            },
            ast::ExpressionInner::Expression(expression) => evaluate_expr(symbol_table, expression),
        }
    }

    pub fn evaluate(program: &ast::Program) {
        let mut symbol_table: HashMap<String, Rc<Value>> = HashMap::new();

        for statement in program {
            match statement {
                ast::Statement::Definition { symbol, expression } => {
                    println!("[runtime] defining symbol: {:#?}", symbol);
                    let value = evaluate_expr(&symbol_table, expression);
                    symbol_table.insert(symbol.clone(), value);
                }
                ast::Statement::Expression(expression) => {
                    println!("[runtime] evaluating free-standing expression");
                    let value = evaluate_expr(&symbol_table, expression);
                    println!("Result: {:#?}", value);
                }
            }
        }

        println!(
            "[runtime] evaluation done, symbol_table state dump: {:#?}",
            symbol_table
        );
    }
}

fn main() {
    use ast::Program;
    use pest::Parser;
    use std::env;
    use std::fs;

    let script_path = env::args().nth(1).expect("no script file specified");

    let unparsed_file = fs::read_to_string(script_path).expect("cannot read file");
    let parse_tree_result = parser::Parser::parse(parser::Rule::program, &unparsed_file);

    if parse_tree_result.is_err() {
        println!("{}", parse_tree_result.unwrap_err());
        return;
    }

    let mut parse_tree = parse_tree_result.unwrap();

    println!("parse tree = {:#?}", parse_tree);
    let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);
    println!("syntax tree = {:#?}", syntax_tree);

    runtime::evaluate(&syntax_tree);
}
