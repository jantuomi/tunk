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
    use pest::Span;

    fn span_into_str(span: Span) -> &str {
        span.as_str()
    }

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
            // println!("Statement::from_pair pair: {:#?}", pair);
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
                Rule::string_literal => ExpressionInner::StringLiteral(string_from_pair(pair)),
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

    #[derive(Debug)]
    pub enum Value {
        Integer(i64),
        String(String),
    }

    fn evaluate_expr(expression: &ast::Expression) -> Value {
        // println!("[runtime] evaluating expression");
        Value::String(String::from("dummy value"))
    }

    pub fn evaluate(program: &ast::Program) {
        let mut symbol_table: HashMap<String, Value> = HashMap::new();

        for statement in program {
            match statement {
                ast::Statement::Definition { symbol, expression } => {
                    println!("[runtime] evaluating definition for symbol: {:#?}", symbol);
                    let value = evaluate_expr(expression);
                    symbol_table.insert(symbol.clone(), value);
                }
                ast::Statement::Expression(expression) => {
                    println!("[runtime] evaluating free-standing expression");
                    let value = evaluate_expr(expression);
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
    use std::fs;

    let unparsed_file = fs::read_to_string("samples/sample1.code").expect("cannot read file");
    let mut parse_tree =
        parser::Parser::parse(parser::Rule::program, &unparsed_file).expect("unsuccessful parse");

    // println!("parse tree = {:#?}", parse_tree);
    let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);
    println!("syntax tree = {:#?}", syntax_tree);

    runtime::evaluate(&syntax_tree);
}
