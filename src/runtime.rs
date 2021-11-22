use super::ast;
use super::builtins;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{} :: Integer", value),
            Value::String(value) => write!(f, "{} :: String", value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Term {
    Variable(usize),
    Abstraction(usize, Rc<Term>),
    Application(Rc<Term>, Rc<Term>),
    Primitive(Value),
    Lazy(String),
}

impl Term {
    fn fmt_with_indent(&self, indent: usize) -> String {
        let indent_str = std::iter::repeat("|  ").take(indent).collect::<String>();
        match self {
            Term::Lazy(symbol) => format!("{}Lazy({})", indent_str, symbol),
            Term::Variable(v) => format!("{}Variable({})", indent_str, v),
            Term::Primitive(value) => match value {
                Value::Integer(int_val) => format!("{}Integer({})", indent_str, int_val),
                Value::String(str_val) => format!("{}String({})", indent_str, str_val),
            },
            Term::Abstraction(v, body) => format!(
                "{}Abstraction({})\n{}",
                indent_str,
                v,
                body.fmt_with_indent(indent + 1)
            ),
            Term::Application(lhs, rhs) => format!(
                "{}Application\n{}\n{}",
                indent_str,
                lhs.fmt_with_indent(indent + 1),
                rhs.fmt_with_indent(indent + 1),
            ),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_with_indent(0))
    }
}

static VAR_ID_INC: AtomicUsize = AtomicUsize::new(0);

pub fn advance_v() -> usize {
    let v = VAR_ID_INC.load(Ordering::Relaxed);
    VAR_ID_INC.store(v + 1, Ordering::Relaxed);
    v
}

// fn try_apply_function(lhs_rc: Rc<Value>, rhs_rc: Rc<Value>, bound_v: Option<usize>) -> Rc<Value> {
//     let lhs = &*lhs_rc;
//     // println!(
//     //     "[debug] try_apply_function({:#?}, {:#?}, {:#?})",
//     //     func_rc, arg_rc, bound_v
//     // );

//     match lhs {
//         Value::Function(func_v, body_rc, builtin_name_opt) => {
//             let v1 = bound_v.unwrap_or(*func_v);
//             let body = &**body_rc;
//             match body {
//                 Value::Var(v2) => {
//                     if v1 == *v2 {
//                         rhs_rc
//                     } else {
//                         Rc::clone(body_rc)
//                     }
//                 }
//                 Value::Function(body_v, _, body_builtin_name_opt) => {
//                     let new_body = try_apply_function(Rc::clone(body_rc), rhs_rc, Some(v1));
//                     // TODO builtin handling
//                     Rc::new(Value::Function(*body_v, new_body, None))
//                 }
//                 _ => Rc::clone(body_rc),
//             }
//         }
//         _ => lhs_rc,
//     }
// }

fn process_expr_inner_unary(
    symbol_table: &HashMap<String, Rc<Term>>,
    inner: &ast::ExpressionInner,
    bound_symbols: &Vec<(ast::Symbol, usize)>,
) -> Rc<Term> {
    match inner {
        ast::ExpressionInner::IntegerLiteral(value) => {
            Rc::new(Term::Primitive(Value::Integer(*value)))
        }
        ast::ExpressionInner::StringLiteral(value) => {
            Rc::new(Term::Primitive(Value::String(value.clone())))
        }
        ast::ExpressionInner::Symbol(value) => {
            let bound_symbol_opt = bound_symbols
                .iter()
                .find(|(bound_symbol, _)| bound_symbol == value);
            if bound_symbol_opt.is_some() {
                return Rc::new(Term::Variable(bound_symbol_opt.unwrap().1));
            }

            let table_lookup_value = symbol_table.get(value);
            if table_lookup_value.is_some() {
                let lookup_rc = table_lookup_value.unwrap();
                return Rc::clone(lookup_rc);
            }

            let builtin_value = builtins::try_builtin_symbol_to_value(value);
            if builtin_value.is_some() {
                return Rc::new(builtin_value.unwrap());
            }

            Rc::new(Term::Lazy(value.clone()))
        }
        ast::ExpressionInner::Expression(value_rc) => {
            let sub_expr = &**value_rc;
            process_expr(symbol_table, sub_expr, bound_symbols)
        }
    }
}

fn process_expr_inner_binary(
    symbol_table: &HashMap<String, Rc<Term>>,
    lhs: &ast::ExpressionInner,
    rhs: &ast::ExpressionInner,
    bound_symbols: &Vec<(ast::Symbol, usize)>,
) -> Rc<Term> {
    match lhs {
        ast::ExpressionInner::Expression(value_rc) => {
            let sub_expr = &**value_rc;

            let lhs_term = process_expr(symbol_table, sub_expr, bound_symbols);
            let rhs_term = process_expr_inner_unary(symbol_table, rhs, bound_symbols);
            Rc::new(Term::Application(lhs_term, rhs_term))
        }
        ast::ExpressionInner::Symbol(value) => {
            let lhs_term: Rc<Term>;
            let bound_symbol_opt = bound_symbols
                .iter()
                .find(|(bound_symbol, _)| bound_symbol == value);

            if let Some(bound_symbol) = bound_symbol_opt {
                lhs_term = Rc::new(Term::Variable(bound_symbol.1));
            } else if let Some(lookup) = symbol_table.get(value) {
                lhs_term = Rc::clone(lookup);
            } else if let Some(builtin) = builtins::try_builtin_symbol_to_value(value) {
                lhs_term = Rc::new(builtin);
            } else {
                lhs_term = Rc::new(Term::Lazy(value.clone()));
            }

            let rhs_term = process_expr_inner_unary(symbol_table, rhs, bound_symbols);
            Rc::new(Term::Application(lhs_term, rhs_term))
        }
        other => unreachable!(
            "[runtime] this should not be on the left side of a binary expression: {:#?}",
            other
        ),
    }
}

fn process_expr(
    symbol_table: &HashMap<String, Rc<Term>>,
    expression: &ast::Expression,
    bound_symbols: &Vec<(ast::Symbol, usize)>,
) -> Rc<Term> {
    match expression {
        ast::Expression::Unary(inner) => {
            process_expr_inner_unary(symbol_table, inner, bound_symbols)
        }
        ast::Expression::Binary(inner1, inner2) => {
            process_expr_inner_binary(symbol_table, inner1, inner2, bound_symbols)
        }
    }
}

pub fn process(program: &ast::Program) {
    let mut symbol_table: HashMap<String, Rc<Term>> = HashMap::new();

    for (index, statement) in program.iter().enumerate() {
        match statement {
            ast::Statement::Definition {
                symbol,
                parameters,
                expression,
            } => {
                println!("[runtime] defining symbol: {:#?}", symbol);
                let bound_params: Vec<(ast::Symbol, usize)> = parameters
                    .iter()
                    .map(|param| (param.clone(), advance_v()))
                    .collect();

                let evaled_expr = process_expr(&symbol_table, expression, &bound_params);
                let mut abstracted_expr: Rc<Term> = evaled_expr;

                // bound_params.iter().rev().for_each(|(_, v)| {
                //     abstracted_expr = Rc::new(Value::Function(*v, Rc::clone(&abstracted_expr)));
                // });

                symbol_table.insert(symbol.clone(), abstracted_expr);
            }
            ast::Statement::Expression(expression) => {
                println!("[runtime] evaluating free-standing expression");
                let term = process_expr(&symbol_table, expression, &vec![]);
                println!("Result:\n{}", term);
            }
        }
    }

    println!(
        "[runtime] evaluation done, symbol_table state dump: {:#?}",
        symbol_table
    );
}
