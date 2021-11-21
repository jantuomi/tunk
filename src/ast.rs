use super::lparser::Rule;
use std::rc::Rc;

pub fn from_parse_tree(parse_tree: &mut pest::iterators::Pairs<Rule>) -> Program {
    let root = parse_tree.next().unwrap();

    match root.as_rule() {
        Rule::program => {
            let statements_with_eoi: Vec<pest::iterators::Pair<Rule>> = root.into_inner().collect();
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
        parameters: Vec<Symbol>,
        expression: Rc<Expression>,
    },
    Expression(Rc<Expression>),
}

impl Statement {
    fn from_pair(pair: pest::iterators::Pair<Rule>) -> Statement {
        let def_or_expr = pair.into_inner().next().unwrap();

        fn definition_from_pair(pair: pest::iterators::Pair<Rule>) -> Statement {
            let mut inner = pair.into_inner();
            let symbol = inner.next().unwrap().as_span().as_str();
            let mut parameters: Vec<Symbol> = vec![];
            let mut expression_opt: Option<Rc<Expression>> = None;

            while let Some(p) = inner.next() {
                match p.as_rule() {
                    Rule::symbol => parameters.push(String::from(p.as_span().as_str())),
                    Rule::expression => {
                        let expression_inners: Vec<ExpressionInner> =
                            p.into_inner().map(ExpressionInner::from_pair).collect();
                        expression_opt = Some(expression_vec_to_tuple(&expression_inners));
                    }
                    _ => panic!(
                        "[ast] illegal rule {:#?} as child of definition",
                        p.as_rule()
                    ),
                }
            }

            let expression =
                expression_opt.expect("[ast] no expression found as child of definition");

            Statement::Definition {
                symbol: String::from(symbol),
                parameters: parameters,
                expression: expression,
            }
        }

        match def_or_expr.as_rule() {
            Rule::definition => definition_from_pair(def_or_expr),
            Rule::expression => {
                let expression_inners: Vec<ExpressionInner> = def_or_expr
                    .into_inner()
                    .map(ExpressionInner::from_pair)
                    .collect();
                Statement::Expression(expression_vec_to_tuple(&expression_inners))
            }
            rule => panic!("[ast] can't make a statement from {:#?}", rule),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionInner {
    Symbol(Symbol),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    Expression(Rc<Expression>),
}

impl ExpressionInner {
    fn from_pair(pair: pest::iterators::Pair<Rule>) -> ExpressionInner {
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
                ExpressionInner::Expression(expression_vec_to_tuple(&expression_inners))
            }
            rule => panic!("[ast] can't make an expression element from {:#?}", rule),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Unary(ExpressionInner),
    Binary(ExpressionInner, ExpressionInner),
}

fn expression_vec_to_tuple(v: &Vec<ExpressionInner>) -> Rc<Expression> {
    match v.len() {
        1 => Rc::new(Expression::Unary(v[0].clone())),
        2 => Rc::new(Expression::Binary(v[0].clone(), v[1].clone())),
        _ => {
            let combined = ExpressionInner::Expression(Rc::new(Expression::Binary(
                v[0].clone(),
                v[1].clone(),
            )));

            let mut new_v = vec![combined];
            for e in &v[2..] {
                new_v.push(e.clone());
            }

            expression_vec_to_tuple(&new_v)
        }
    }
}

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
