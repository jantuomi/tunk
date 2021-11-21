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
    use std::rc::Rc;

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
                let expression = inner.next().unwrap();
                let expression_inners: Vec<ExpressionInner> = expression
                    .into_inner()
                    .map(ExpressionInner::from_pair)
                    .collect();

                Statement::Definition {
                    symbol: String::from(symbol),
                    expression: expression_vec_to_tuple(&expression_inners),
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
}

mod runtime {
    use super::ast;
    use std::collections::HashMap;
    use std::fmt;
    use std::rc::Rc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[derive(Debug, Clone)]
    pub enum BuiltinFunction {
        IntegerIncrement,
        IntegerAdd,
        IntegerAdd1(i64),
    }

    #[derive(Debug, Clone)]
    pub enum Value {
        Integer(i64),
        String(String),
        Var(usize),
        Function(usize, Rc<Value>),
        BuiltinFunction(BuiltinFunction),
    }

    impl fmt::Display for Value {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Value::Integer(value) => write!(f, "{} :: Integer", value),
                Value::String(value) => write!(f, "{} :: String", value),
                Value::Var(v) => write!(f, "{} :: Unbound variable", v),
                Value::Function(_, _) => {
                    write!(f, "Function :: Function")
                }
                Value::BuiltinFunction(_) => write!(f, "Built-in function :: Function"),
            }
        }
    }

    static VAR_ID_INC: AtomicUsize = AtomicUsize::new(0);
    const B_INTEGER_INCREMENT: &str = "int.increment";
    const B_INTEGER_ADD: &str = "int.add";

    fn advance_v() -> usize {
        let v = VAR_ID_INC.load(Ordering::Relaxed);
        VAR_ID_INC.store(v + 1, Ordering::Relaxed);
        v
    }

    fn try_builtin_symbol_to_value(symbol: &ast::Symbol) -> Option<Value> {
        match symbol.as_str() {
            "true" => {
                let x = advance_v();
                let y = advance_v();
                Some(Value::Function(
                    x,
                    Rc::new(Value::Function(y, Rc::new(Value::Var(x)))),
                ))
            }
            "false" => {
                let x = advance_v();
                let y = advance_v();
                Some(Value::Function(
                    x,
                    Rc::new(Value::Function(y, Rc::new(Value::Var(y)))),
                ))
            }
            "id" => {
                let v = advance_v();
                Some(Value::Function(v, Rc::new(Value::Var(v))))
            }
            B_INTEGER_INCREMENT => Some(Value::BuiltinFunction(BuiltinFunction::IntegerIncrement)),
            B_INTEGER_ADD => Some(Value::BuiltinFunction(BuiltinFunction::IntegerAdd)),
            _ => None,
        }
    }

    fn try_apply_function(
        func_rc: Rc<Value>,
        arg_rc: Rc<Value>,
        bound_v: Option<usize>,
    ) -> Rc<Value> {
        let func = &*func_rc;
        let arg = &*arg_rc;
        match func {
            Value::Function(func_v, body_rc) => {
                let v1 = bound_v.unwrap_or(*func_v);
                let body = &**body_rc;
                match body {
                    Value::Var(v2) => {
                        if v1 == *v2 {
                            arg_rc
                        } else {
                            Rc::clone(body_rc)
                        }
                    }
                    Value::Function(body_v, _) => {
                        let new_body = try_apply_function(Rc::clone(body_rc), arg_rc, Some(v1));
                        Rc::new(Value::Function(*body_v, new_body))
                    }
                    _ => Rc::clone(body_rc),
                }
            }
            Value::BuiltinFunction(builtin) => match builtin {
                BuiltinFunction::IntegerIncrement => match arg {
                    Value::Integer(value) => Rc::new(Value::Integer(value + 1)),
                    _ => panic!(
                        "[runtime] tried to apply non-integer value to {}",
                        B_INTEGER_INCREMENT
                    ),
                },
                BuiltinFunction::IntegerAdd => match arg {
                    Value::Integer(value) => {
                        Rc::new(Value::BuiltinFunction(BuiltinFunction::IntegerAdd1(*value)))
                    }
                    _ => panic!(
                        "[runtime] tried to apply non-integer value to {}",
                        B_INTEGER_ADD
                    ),
                },
                BuiltinFunction::IntegerAdd1(other) => match arg {
                    Value::Integer(value) => Rc::new(Value::Integer(other + value)),
                    _ => panic!(
                        "[runtime] tried to apply non-integer value to {}",
                        B_INTEGER_ADD
                    ),
                },
            },
            _ => func_rc,
        }
    }

    fn evaluate_expr_inner_unary(
        symbol_table: &HashMap<String, Rc<Value>>,
        inner: &ast::ExpressionInner,
    ) -> Rc<Value> {
        match inner {
            ast::ExpressionInner::IntegerLiteral(value) => Rc::new(Value::Integer(*value)),
            ast::ExpressionInner::StringLiteral(value) => Rc::new(Value::String(value.clone())),
            ast::ExpressionInner::Symbol(value) => {
                let builtin_value = try_builtin_symbol_to_value(value);
                if builtin_value.is_some() {
                    return Rc::new(builtin_value.unwrap());
                }

                let table_lookup_value = symbol_table.get(value);
                if table_lookup_value.is_some() {
                    let lookup_rc = table_lookup_value.unwrap();
                    return Rc::clone(lookup_rc);
                }

                panic!("[runtime] symbol not defined: {:#?}", value);
            }
            ast::ExpressionInner::Expression(value_rc) => {
                let sub_expr = &**value_rc;
                evaluate_expr(symbol_table, sub_expr)
            }
        }
    }

    fn evaluate_expr_inner_binary(
        symbol_table: &HashMap<String, Rc<Value>>,
        lhs: &ast::ExpressionInner,
        rhs: &ast::ExpressionInner,
    ) -> Rc<Value> {
        match lhs {
            ast::ExpressionInner::Expression(value_rc) => {
                let sub_expr = &**value_rc;

                let evaled_lhs = evaluate_expr(symbol_table, sub_expr);
                let evaled_rhs = evaluate_expr_inner_unary(symbol_table, rhs);
                try_apply_function(evaled_lhs, evaled_rhs, None)
            }
            ast::ExpressionInner::Symbol(value) => {
                let lhs_value_rc: Rc<Value>;

                if let Some(builtin) = try_builtin_symbol_to_value(value) {
                    lhs_value_rc = Rc::new(builtin);
                } else if let Some(lookup) = symbol_table.get(value) {
                    lhs_value_rc = Rc::clone(lookup);
                } else {
                    panic!("[runtime] symbol not defined: {:#?}", value);
                }

                let evaled_rhs = evaluate_expr_inner_unary(symbol_table, rhs);
                try_apply_function(lhs_value_rc, evaled_rhs, None)
            }
            other => unreachable!(
                "[runtime] this should not be on the left side of a binary expression: {:#?}",
                other
            ),
        }
    }

    fn evaluate_expr(
        symbol_table: &HashMap<String, Rc<Value>>,
        expression: &ast::Expression,
    ) -> Rc<Value> {
        match expression {
            ast::Expression::Unary(inner) => evaluate_expr_inner_unary(symbol_table, inner),
            ast::Expression::Binary(inner1, inner2) => {
                evaluate_expr_inner_binary(symbol_table, inner1, inner2)
            }
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
                    println!("Result: {}", value);
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

    // println!("parse tree = {:#?}", parse_tree);
    let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);
    // println!("syntax tree = {:#?}", syntax_tree);

    runtime::evaluate(&syntax_tree);
}
