use super::ast;
use super::runtime::{advance_v, Term};
use std::rc::Rc;

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

pub fn try_builtin_symbol_to_value(symbol: &ast::Symbol) -> Option<Term> {
    match symbol.as_str() {
        "true" => Some(make_boolean_true_function()),
        "false" => Some(make_boolean_false_function()),
        "id" => Some(make_identity_function()),
        _ => None,
    }
}
