use super::runtime::{advance_v, Term, Value};
use super::*;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Builtin {
    pub identifier: &'static str,
    pub arguments: Vec<Value>,
    pub n_arguments: usize,
}

impl Builtin {
    pub fn new(identifier: &'static str, n_arguments: usize) -> Builtin {
        Builtin {
            identifier,
            arguments: vec![],
            n_arguments,
        }
    }

    pub fn bind_arg(&self, arg: &Value) -> Builtin {
        let mut new_args = self.arguments.clone();
        new_args.push(arg.clone());

        Builtin {
            identifier: self.identifier,
            arguments: new_args,
            n_arguments: self.n_arguments - 1,
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut arg_str = String::new();
        for argument in &self.arguments {
            arg_str.push_str(format!(" {}", argument).as_str());
        }

        write!(f, "Builtin({}){}", self.identifier, arg_str)
    }
}

fn argument_type_error(builtin_id: &'static str, arg: &Term) -> Result<(Rc<Term>, usize), String> {
    Err(format!(
        "[runtime] cannot apply builtin {} to argument {}",
        builtin_id, arg
    ))
}

fn argument_n_error(builtin_id: &'static str, n: usize) -> Result<(Rc<Term>, usize), String> {
    Err(format!(
        "[runtime] builtin {} can not accept an argument with n {}",
        builtin_id, n
    ))
}

macro_rules! boolean_function {
    ($value:expr) => {
        match $value {
            true => make_boolean_true_function(),
            false => make_boolean_false_function(),
        }
    };
}

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

pub const B_INTEGER_EQ: &str = "int.eq?";
pub const B_INTEGER_INCREMENT: &str = "int.inc";
pub const B_INTEGER_ADD: &str = "int.add";
pub const B_INTEGER_SUBTRACT: &str = "int.sub";
pub const B_INTEGER_MULTIPLY: &str = "int.mul";
pub const B_INTEGER_DIVIDE: &str = "int.div";
pub const B_STRING_EQ: &str = "string.eq?";
pub const B_BOOL_TO_STRING: &str = "bool.to-string";

pub fn try_ast_symbol_to_builtin_term(symbol: &str) -> Option<Term> {
    let builtin = match symbol {
        "true" => return Some(make_boolean_true_function()),
        "false" => return Some(make_boolean_false_function()),
        "id" => return Some(make_identity_function()),
        B_INTEGER_EQ => Builtin::new(B_INTEGER_EQ, 2),
        B_INTEGER_INCREMENT => Builtin::new(B_INTEGER_INCREMENT, 1),
        B_INTEGER_ADD => Builtin::new(B_INTEGER_ADD, 2),
        B_INTEGER_SUBTRACT => Builtin::new(B_INTEGER_SUBTRACT, 2),
        B_INTEGER_MULTIPLY => Builtin::new(B_INTEGER_MULTIPLY, 2),
        B_INTEGER_DIVIDE => Builtin::new(B_INTEGER_DIVIDE, 2),
        B_STRING_EQ => Builtin::new(B_STRING_EQ, 2),
        B_BOOL_TO_STRING => Builtin::new(B_BOOL_TO_STRING, 1),
        _ => return None,
    };

    Some(Term::Builtin(builtin))
}

pub fn evaluate_builtin(builtin: &Builtin, rhs: Rc<Term>) -> Result<(Rc<Term>, usize), String> {
    let result_term = match builtin.identifier {
        B_INTEGER_EQ => match &*rhs {
            Term::Primitive(primitive @ Value::Integer(value)) => match builtin.n_arguments {
                2 => Term::Builtin(builtin.bind_arg(primitive)),
                1 => {
                    let other_value = extract_enum_value!(&builtin.arguments[0], Value::Integer(other_value) => other_value);

                    boolean_function!(value == other_value)
                }
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_INCREMENT => match &*rhs {
            Term::Primitive(Value::Integer(value)) => Term::Primitive(Value::Integer(value + 1)),
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_ADD => match &*rhs {
            Term::Primitive(primitive @ Value::Integer(value)) => match builtin.n_arguments {
                2 => Term::Builtin(builtin.bind_arg(primitive)),
                1 => {
                    let other_value = extract_enum_value!(&builtin.arguments[0], Value::Integer(other_value) => other_value);
                    Term::Primitive(Value::Integer(other_value + value))
                }
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_SUBTRACT => match &*rhs {
            Term::Primitive(primitive @ Value::Integer(value)) => match builtin.n_arguments {
                2 => Term::Builtin(builtin.bind_arg(primitive)),
                1 => {
                    let other_value = extract_enum_value!(&builtin.arguments[0], Value::Integer(other_value) => other_value);
                    Term::Primitive(Value::Integer(other_value - value))
                }
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_MULTIPLY => match &*rhs {
            Term::Primitive(primitive @ Value::Integer(value)) => match builtin.n_arguments {
                2 => Term::Builtin(builtin.bind_arg(primitive)),
                1 => {
                    let other_value = extract_enum_value!(&builtin.arguments[0], Value::Integer(other_value) => other_value);
                    Term::Primitive(Value::Integer(other_value * value))
                }
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_INTEGER_DIVIDE => match &*rhs {
            Term::Primitive(primitive @ Value::Integer(value)) => match builtin.n_arguments {
                2 => Term::Builtin(builtin.bind_arg(primitive)),
                1 => {
                    if *value == 0 {
                        return Err("[runtime] divide by zero".to_owned());
                    }
                    let other_value = extract_enum_value!(&builtin.arguments[0], Value::Integer(other_value) => other_value);
                    Term::Primitive(Value::Integer(other_value / value))
                }
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_STRING_EQ => match &*rhs {
            Term::Primitive(primitive @ Value::String(value)) => match builtin.n_arguments {
                2 => Term::Builtin(builtin.bind_arg(primitive)),
                1 => {
                    let other_value = extract_enum_value!(&builtin.arguments[0], Value::String(other_value) => other_value);

                    boolean_function!(value == other_value)
                }
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
            _ => return Ok((Rc::new(Term::Builtin(builtin.clone())), 0)),
        },
        B_BOOL_TO_STRING => {
            let true_str_rc = Rc::new(Term::Primitive(Value::String("true".to_owned())));
            let false_str_rc = Rc::new(Term::Primitive(Value::String("false".to_owned())));
            let inner = Rc::new(Term::Application(rhs, true_str_rc));
            Term::Application(inner, false_str_rc)
        }
        _ => {
            return Err(format!(
                "[runtime] invalid builtin evaluated: {}",
                builtin.identifier
            ))
        }
    };

    Ok((Rc::new(result_term), 1))
}
