use super::ast;
use super::builtins;
use super::builtins::BuiltinFunction;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

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

fn advance_v() -> usize {
    let v = VAR_ID_INC.load(Ordering::Relaxed);
    VAR_ID_INC.store(v + 1, Ordering::Relaxed);
    v
}

pub fn make_boolean_true_function() -> Value {
    let x = advance_v();
    let y = advance_v();
    Value::Function(x, Rc::new(Value::Function(y, Rc::new(Value::Var(x)))))
}

pub fn make_boolean_false_function() -> Value {
    let x = advance_v();
    let y = advance_v();
    Value::Function(x, Rc::new(Value::Function(y, Rc::new(Value::Var(y)))))
}

pub fn make_identity_function() -> Value {
    let v = advance_v();
    Value::Function(v, Rc::new(Value::Var(v)))
}

fn try_apply_function(func_rc: Rc<Value>, arg_rc: Rc<Value>, bound_v: Option<usize>) -> Rc<Value> {
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
        Value::BuiltinFunction(builtin) => builtins::apply_builtin(builtin, arg),
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
            let builtin_value = builtins::try_builtin_symbol_to_value(value);
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

            if let Some(builtin) = builtins::try_builtin_symbol_to_value(value) {
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
            ast::Statement::Definition {
                symbol,
                parameters,
                expression,
            } => {
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
