use super::ast;
use super::runtime;
use super::runtime::Value;
use std::rc::Rc;

pub const B_INTEGER_INCREMENT: &str = "int.increment";
pub const B_INTEGER_DECREMENT: &str = "int.decrement";
pub const B_INTEGER_ADD: &str = "int.add";
pub const B_INTEGER_MULTIPLY: &str = "int.multiply";
pub const B_INTEGER_EQ: &str = "int.eq?";

#[derive(Debug, Clone)]
pub enum BuiltinFunction {
    IntegerIncrement,
    IntegerDecrement,
    IntegerAdd,
    IntegerAdd1(i64),
    IntegerMultiply,
    IntegerMultiply1(i64),
    IntegerEq,
    IntegerEq1(i64),
}

pub fn try_builtin_symbol_to_value(symbol: &ast::Symbol) -> Option<Value> {
    match symbol.as_str() {
        "true" => Some(runtime::make_boolean_true_function()),
        "false" => Some(runtime::make_boolean_false_function()),
        "id" => Some(runtime::make_identity_function()),
        B_INTEGER_INCREMENT => Some(Value::BuiltinFunction(BuiltinFunction::IntegerIncrement)),
        B_INTEGER_DECREMENT => Some(Value::BuiltinFunction(BuiltinFunction::IntegerDecrement)),
        B_INTEGER_ADD => Some(Value::BuiltinFunction(BuiltinFunction::IntegerAdd)),
        B_INTEGER_MULTIPLY => Some(Value::BuiltinFunction(BuiltinFunction::IntegerMultiply)),
        B_INTEGER_EQ => Some(Value::BuiltinFunction(BuiltinFunction::IntegerEq)),
        _ => None,
    }
}

pub fn apply_builtin(builtin: &BuiltinFunction, arg: &Value) -> Rc<Value> {
    match builtin {
        BuiltinFunction::IntegerIncrement => match arg {
            Value::Integer(value) => Rc::new(Value::Integer(value + 1)),
            _ => panic!(
                "[runtime] tried to apply non-integer value to {}",
                B_INTEGER_INCREMENT
            ),
        },
        BuiltinFunction::IntegerDecrement => match arg {
            Value::Integer(value) => Rc::new(Value::Integer(value - 1)),
            _ => panic!(
                "[runtime] tried to apply non-integer value to {}",
                B_INTEGER_DECREMENT
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
        BuiltinFunction::IntegerMultiply => match arg {
            Value::Integer(value) => Rc::new(Value::BuiltinFunction(
                BuiltinFunction::IntegerMultiply1(*value),
            )),
            _ => panic!(
                "[runtime] tried to apply non-integer value to {}",
                B_INTEGER_MULTIPLY
            ),
        },
        BuiltinFunction::IntegerMultiply1(other) => match arg {
            Value::Integer(value) => Rc::new(Value::Integer(other * value)),
            _ => panic!(
                "[runtime] tried to apply non-integer value to {}",
                B_INTEGER_MULTIPLY
            ),
        },
        BuiltinFunction::IntegerEq => match arg {
            Value::Integer(value) => {
                Rc::new(Value::BuiltinFunction(BuiltinFunction::IntegerEq1(*value)))
            }
            _ => panic!(
                "[runtime] tried to apply non-integer value to {}",
                B_INTEGER_EQ
            ),
        },
        BuiltinFunction::IntegerEq1(other) => match arg {
            Value::Integer(value) => Rc::new(if other == value {
                runtime::make_boolean_true_function()
            } else {
                runtime::make_boolean_false_function()
            }),
            _ => panic!(
                "[runtime] tried to apply non-integer value to {}",
                B_INTEGER_EQ
            ),
        },
    }
}
