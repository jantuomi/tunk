use super::ast;
use super::runtime::{advance_v, Term, Value};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Builtin {
    pub identifier: &'static str,
    pub repr_name: &'static str,
    pub arguments: Vec<Value>,
}

impl Builtin {
    pub fn new(identifier: &'static str) -> Builtin {
        Builtin {
            identifier: identifier,
            repr_name: identifier,
            arguments: vec![],
        }
    }

    // pub fn bind_arg(self: &Builtin, arg: &Value) -> Builtin {
    //     let mut new_args = self.arguments.clone();
    //     new_args.push(arg.clone());

    //     Builtin {
    //         identifier: new_identifier,
    //         repr_name: self.repr_name,
    //         arguments: new_args,
    //     }
    // }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut arg_str = String::new();
        for argument in &self.arguments {
            arg_str.push_str(format!(" {}", argument).as_str());
        }

        write!(f, "Builtin({}){}", self.repr_name, arg_str)
    }
}

fn argument_error(builtin_id: &'static str, arg: &Value) -> Result<(Rc<Term>, usize), String> {
    Err(format!(
        "[runtime] cannot apply builtin {} to argument {}",
        builtin_id, arg
    ))
}

pub const B_INTEGER_EQ: &str = "int.eq?";
pub const B_INTEGER_EQ_1: &str = "##int.eq?_1";
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
    let builtin = match symbol.as_str() {
        "true" => return Some(make_boolean_true_function()),
        "false" => return Some(make_boolean_false_function()),
        "id" => return Some(make_identity_function()),
        B_INTEGER_EQ => Builtin::new(B_INTEGER_EQ),
        B_INTEGER_INCREMENT => Builtin::new(B_INTEGER_INCREMENT),
        B_INTEGER_ADD => Builtin::new(B_INTEGER_ADD),
        _ => return None,
    };

    Some(Term::Builtin(builtin))
}

pub fn evaluate_builtin(builtin: &Builtin, rhs: Rc<Term>) -> Result<(Rc<Term>, usize), String> {
    let result_term = match builtin.identifier {
        B_INTEGER_EQ => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(_) => {
                    let new_builtin = Builtin {
                        identifier: B_INTEGER_EQ_1,
                        repr_name: builtin.identifier,
                        arguments: vec![primitive.clone()],
                    };
                    Term::Builtin(new_builtin)
                }
                other => return argument_error(builtin.identifier, other),
            },
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_EQ_1 => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => {
                    assert_eq!(builtin.arguments.len(), 1);
                    let other = &builtin.arguments[0];
                    match other {
                        Value::Integer(other_value) => {
                            if value == other_value {
                                make_boolean_true_function()
                            } else {
                                make_boolean_false_function()
                            }
                        }
                        other => return argument_error(builtin.identifier, other),
                    }
                }
                other => return argument_error(builtin.identifier, other),
            },
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_INCREMENT => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => Term::Primitive(Value::Integer(value + 1)),
                other => return argument_error(builtin.identifier, other),
            },
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_ADD => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(_) => {
                    let new_builtin = Builtin {
                        identifier: B_INTEGER_ADD_1,
                        repr_name: builtin.identifier,
                        arguments: vec![primitive.clone()],
                    };
                    Term::Builtin(new_builtin)
                }
                other => return argument_error(builtin.identifier, other),
            },
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_ADD_1 => match &*rhs {
            Term::Primitive(primitive) => match primitive {
                Value::Integer(value) => {
                    assert_eq!(builtin.arguments.len(), 1);
                    let summand = &builtin.arguments[0];
                    match summand {
                        Value::Integer(summand_value) => {
                            Term::Primitive(Value::Integer(summand_value + value))
                        }
                        other => return argument_error(builtin.identifier, other),
                    }
                }
                other => return argument_error(builtin.identifier, other),
            },
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        _ => {
            return Err(format!(
                "[runtime] invalid builtin evaluated: {}",
                builtin.identifier
            ))
        }
    };

    Ok((Rc::new(result_term), 1))
}
