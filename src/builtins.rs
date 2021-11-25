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

        write!(f, "Builtin({}{})", self.identifier, arg_str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    pub identifier: &'static str,
    pub term: Rc<Term>,
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
            true => Term::Builtin(Builtin::new(B_TRUE, 0)),
            false => Term::Builtin(Builtin::new(B_FALSE, 0)),
        }
    };
}

pub const B_TRUE: &str = "true";
pub const B_FALSE: &str = "false";
pub const B_ID: &str = "id";
pub const B_INTEGER_EQ: &str = "int.eq?";
pub const B_INTEGER_INCREMENT: &str = "int.inc";
pub const B_INTEGER_ADD: &str = "int.add";
pub const B_INTEGER_SUBTRACT: &str = "int.sub";
pub const B_INTEGER_MULTIPLY: &str = "int.mul";
pub const B_INTEGER_DIVIDE: &str = "int.div";
pub const B_STRING_EQ: &str = "string.eq?";
pub const B_BOOL_TO_STRING: &str = "bool.to-string";
pub const B_COMMAND_PRINTLN: &str = "cmd.println!";

pub const C_PRINTLN: &str = "PrintLn";

pub fn try_ast_symbol_to_builtin_term(symbol: &str) -> Option<Term> {
    let builtin = match symbol {
        B_TRUE => Builtin::new(B_TRUE, 0),
        B_FALSE => Builtin::new(B_FALSE, 0),
        B_ID => Builtin::new(B_ID, 0),
        B_INTEGER_EQ => Builtin::new(B_INTEGER_EQ, 2),
        B_INTEGER_INCREMENT => Builtin::new(B_INTEGER_INCREMENT, 1),
        B_INTEGER_ADD => Builtin::new(B_INTEGER_ADD, 2),
        B_INTEGER_SUBTRACT => Builtin::new(B_INTEGER_SUBTRACT, 2),
        B_INTEGER_MULTIPLY => Builtin::new(B_INTEGER_MULTIPLY, 2),
        B_INTEGER_DIVIDE => Builtin::new(B_INTEGER_DIVIDE, 2),
        B_STRING_EQ => Builtin::new(B_STRING_EQ, 2),
        B_BOOL_TO_STRING => Builtin::new(B_BOOL_TO_STRING, 1),
        B_COMMAND_PRINTLN => Builtin::new(B_COMMAND_PRINTLN, 1),
        _ => return None,
    };

    Some(Term::Builtin(builtin))
}

pub fn evaluate_builtin(builtin: &Builtin, rhs: Rc<Term>) -> Result<(Rc<Term>, usize), String> {
    let result_term = match builtin.identifier {
        B_TRUE => {
            let y = advance_v();
            Term::Abstraction(y, rhs)
        }
        B_FALSE => {
            let y = advance_v();
            Term::Abstraction(y, Rc::new(Term::Variable(y)))
        }
        B_ID => return Ok((rhs, 1)),
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
        B_COMMAND_PRINTLN => match &*rhs {
            Term::Primitive(Value::String(_)) => match builtin.n_arguments {
                1 => Term::Command(C_PRINTLN, rhs),
                _ => return argument_n_error(builtin.identifier, builtin.n_arguments),
            },
            Term::Primitive(_) => return argument_type_error(builtin.identifier, &*rhs),
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

pub fn perform_command(command_term: &Term) -> Result<(), String> {
    let (command, term) = extract_enum_value!(command_term, Term::Command(c, t) => (c, t));

    match (*command, &**term) {
        (builtins::C_PRINTLN, Term::Primitive(Value::String(str_val))) => println!("{}", str_val),
        _ => return Err(format!("[runtime] invalid command: {}", command)),
    }

    Ok(())
}
