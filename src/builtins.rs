use super::ast;
use super::runtime::{advance_v, Term, Value};
use std::rc::Rc;

pub const B_INTEGER_EQ0: &str = "int.eq0?";
pub const B_INTEGER_INCREMENT: &str = "int.increment";

pub fn make_boolean_true_function() -> Term {
    let x = advance_v();
    let y = advance_v();
    Term::Abstraction(x, Rc::new(Term::Abstraction(y, Rc::new(Term::Variable(x)))))
}

pub fn make_boolean_false_function() -> Term {
    let x = advance_v();
    let y = advance_v();
    Term::Abstraction(x, Rc::new(Term::Abstraction(y, Rc::new(Term::Variable(y)))))
}

pub fn make_identity_function() -> Term {
    let v = advance_v();
    Term::Abstraction(v, Rc::new(Term::Variable(v)))
}

pub fn try_ast_symbol_to_builtin_term(symbol: &ast::Symbol) -> Option<Term> {
    match symbol.as_str() {
        "true" => Some(make_boolean_true_function()),
        "false" => Some(make_boolean_false_function()),
        "id" => Some(make_identity_function()),
        B_INTEGER_EQ0 => Some(Term::Builtin(String::from(B_INTEGER_EQ0))),
        B_INTEGER_INCREMENT => Some(Term::Builtin(String::from(B_INTEGER_INCREMENT))),
        _ => None,
    }
}

pub fn evaluate_builtin(symbol: &String, rhs: Rc<Term>) -> Result<(Rc<Term>, usize), String> {
    let result_term = match symbol.as_str() {
        B_INTEGER_EQ0 => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => {
                    if *value == 0 {
                        make_boolean_true_function()
                    } else {
                        make_boolean_false_function()
                    }
                }
                other => {
                    return Err(format!(
                        "[runtime] cannot apply builtin {} to argument type {}",
                        symbol, other
                    ))
                }
            },
            _ => return Ok((Rc::new(Term::Builtin(symbol.clone())), 0)),
        },
        B_INTEGER_INCREMENT => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => Term::Primitive(Value::Integer(value + 1)),
                other => {
                    return Err(format!(
                        "[runtime] cannot apply builtin {} to argument type {}",
                        symbol, other
                    ))
                }
            },
            _ => return Ok((Rc::new(Term::Builtin(symbol.clone())), 0)),
        },
        _ => return Err(format!("[runtime] invalid builtin evaluated: {}", symbol)),
    };

    Ok((Rc::new(result_term), 1))
}
