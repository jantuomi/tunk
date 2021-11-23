use super::ast;
use super::runtime::{advance_v, Term, Value};
use std::rc::Rc;

pub const B_INTEGER_EQ0: &str = "int.eq0?";
pub const B_INTEGER_INCREMENT: &str = "int.increment";
pub const B_INTEGER_ADD: &str = "int.add";
pub const B_INTEGER_ADD_1: &str = "##int.add_1";

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
        B_INTEGER_EQ0 => Some(Term::Builtin(String::from(B_INTEGER_EQ0), vec![])),
        B_INTEGER_INCREMENT => Some(Term::Builtin(String::from(B_INTEGER_INCREMENT), vec![])),
        B_INTEGER_ADD => Some(Term::Builtin(String::from(B_INTEGER_ADD), vec![])),
        _ => None,
    }
}

pub fn evaluate_builtin(
    symbol: &String,
    bound_arguments: &Vec<Value>,
    rhs: Rc<Term>,
) -> Result<(Rc<Term>, usize), String> {
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
                        "[runtime] cannot apply builtin {} to argument {}",
                        symbol, other
                    ))
                }
            },
            _ => return Ok((Rc::new(Term::Builtin(symbol.clone(), vec![])), 0)),
        },
        B_INTEGER_INCREMENT => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => Term::Primitive(Value::Integer(value + 1)),
                other => {
                    return Err(format!(
                        "[runtime] cannot apply builtin {} to argument {}",
                        symbol, other
                    ))
                }
            },
            _ => return Ok((Rc::new(Term::Builtin(symbol.clone(), vec![])), 0)),
        },
        B_INTEGER_ADD => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(_) => {
                    Term::Builtin(String::from(B_INTEGER_ADD_1), vec![primitive.clone()])
                }
                other => {
                    return Err(format!(
                        "[runtime] cannot apply builtin {} to argument {}",
                        symbol, other
                    ))
                }
            },
            _ => return Ok((Rc::new(Term::Builtin(symbol.clone(), vec![])), 0)),
        },
        B_INTEGER_ADD_1 => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => {
                    assert_eq!(bound_arguments.len(), 1);
                    let summand = &bound_arguments[0];
                    match summand {
                        Value::Integer(summand_value) => {
                            Term::Primitive(Value::Integer(summand_value + value))
                        }
                        other => {
                            return Err(format!(
                                "[runtime] cannot apply builtin {} to argument {}",
                                symbol, other
                            ))
                        }
                    }
                }
                other => {
                    return Err(format!(
                        "[runtime] cannot apply builtin {} to argument {}",
                        symbol, other
                    ))
                }
            },
            _ => {
                return Ok((
                    Rc::new(Term::Builtin(symbol.clone(), bound_arguments.clone())),
                    0,
                ))
            }
        },
        _ => return Err(format!("[runtime] invalid builtin evaluated: {}", symbol)),
    };

    Ok((Rc::new(result_term), 1))
}
