use super::ast;
use super::builtins;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "Integer({})", value),
            Value::String(value) => write!(f, "String(\"{}\")", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Variable(usize),
    Abstraction(usize, Rc<Term>),
    Application(Rc<Term>, Rc<Term>),
    Primitive(Value),
    Lazy(String),
    Builtin(builtins::Builtin),
    Command(&'static str, Rc<Term>),
}

impl Term {
    fn fmt_with_indent(&self, indent: usize) -> String {
        let indent_str = "|  ".repeat(indent);
        match self {
            Term::Lazy(symbol) => format!("{}Lazy({})", indent_str, symbol),
            Term::Variable(v) => format!("{}Variable({})", indent_str, v),
            Term::Primitive(value) => format!("{}{}", indent_str, value),
            Term::Builtin(builtin) => format!("{}{}", indent_str, builtin),
            Term::Command(command, term_rc) => {
                format!("{}Command({}, {})", indent_str, command, term_rc)
            }
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

pub static VAR_ID_INC: AtomicUsize = AtomicUsize::new(0);

pub fn advance_v() -> usize {
    let v = VAR_ID_INC.load(Ordering::Relaxed);
    VAR_ID_INC.store(v + 1, Ordering::Relaxed);
    v
}

fn _bound_variable_opt_to_string(bound_variable_opt: &Option<(usize, Rc<Term>)>) -> String {
    match bound_variable_opt {
        Some((v, term_rc)) => format!("({}, {})", v, term_rc),
        None => "None".to_owned(),
    }
}

fn substitute_var(
    term_rc: Rc<Term>,
    bound_variable_opt: &Option<(usize, Rc<Term>)>,
) -> ReductionResult {
    let term = &*term_rc;
    match term {
        Term::Variable(v) => match bound_variable_opt {
            Some((bound_v, bound_term_rc)) => {
                if bound_v == v {
                    Ok((Rc::clone(bound_term_rc), 1))
                } else {
                    Ok((term_rc, 0))
                }
            }
            None => Ok((term_rc, 0)),
        },
        _ => Err(format!(
            "[runtime] non-var term passed to substitute_var: {}",
            term_rc
        )),
    }
}

pub type ReductionResult = Result<(Rc<Term>, usize), String>;

pub fn reduce_term(
    symbol_table: &HashMap<String, Rc<Term>>,
    term_rc: Rc<Term>,
    bound_variable_opt: &Option<(usize, Rc<Term>)>,
    resolve_lazy: bool,
) -> ReductionResult {
    let term = &*term_rc;

    let (result_term, result_n) = match term {
        Term::Primitive(_) => Ok((Rc::clone(&term_rc), 0)),
        Term::Command(_, _) => Ok((Rc::clone(&term_rc), 0)),
        Term::Variable(_) => substitute_var(Rc::clone(&term_rc), bound_variable_opt),
        Term::Application(lhs_rc, rhs_rc) => {
            let (subst_rhs_rc, rhs_n) = reduce_term(
                symbol_table,
                Rc::clone(rhs_rc),
                bound_variable_opt,
                resolve_lazy,
            )?;

            let (result, app_n) = match &**lhs_rc {
                Term::Abstraction(abs_v, abs_body_rc) => {
                    let (reduced_term, reduced_n) = reduce_term(
                        symbol_table,
                        Rc::clone(abs_body_rc),
                        &Some((*abs_v, subst_rhs_rc)),
                        resolve_lazy,
                    )?;

                    let result: ReductionResult = Ok((reduced_term, reduced_n + 1));
                    result
                }
                Term::Builtin(builtin) => {
                    let (builtin_evaled_terms, builtin_n) =
                        builtins::evaluate_builtin(builtin, Rc::clone(&subst_rhs_rc))?;

                    // TODO: think this through
                    if builtin_n > 0 {
                        Ok((builtin_evaled_terms, rhs_n + builtin_n))
                    } else {
                        let new_app =
                            Term::Application(Rc::clone(lhs_rc), Rc::clone(&subst_rhs_rc));
                        Ok((Rc::new(new_app), rhs_n))
                    }
                }
                _ => {
                    let (subst_lhs_rc, lhs_n) = reduce_term(
                        symbol_table,
                        Rc::clone(lhs_rc),
                        bound_variable_opt,
                        resolve_lazy,
                    )?;

                    let subst_n = rhs_n + lhs_n;
                    if subst_n > 0 {
                        let new_app = Term::Application(subst_lhs_rc, subst_rhs_rc);
                        Ok((Rc::new(new_app), subst_n))
                    } else {
                        let new_app =
                            Term::Application(Rc::clone(lhs_rc), Rc::clone(&subst_rhs_rc));
                        Ok((Rc::new(new_app), 0))
                    }
                }
            }?;

            Ok((result, app_n))
        }
        Term::Abstraction(abs_v, body_rc) => {
            let (subst_body, subst_n) = reduce_term(
                symbol_table,
                Rc::clone(body_rc),
                bound_variable_opt,
                resolve_lazy,
            )?;

            if subst_n > 0 {
                let new_abs = Term::Abstraction(*abs_v, subst_body);

                Ok((Rc::new(new_abs), subst_n))
            } else {
                Ok((Rc::clone(&term_rc), 0))
            }
        }
        Term::Builtin(_) => Ok((Rc::clone(&term_rc), 0)),
        Term::Lazy(symbol) => {
            // TODO: alpha conversion
            // Lift all substituted vs by current v_inc, and then update v_inc to match
            if resolve_lazy {
                let table_lookup_value = symbol_table
                    .get(symbol)
                    .ok_or(format!("[runtime] symbol not defined: {}", symbol))?;

                // println!("Expanded {} into\n{}\n", symbol, *table_lookup_value);
                Ok((Rc::clone(table_lookup_value), 1))
            } else {
                Ok((term_rc, 0))
            }
        }
    }?;

    Ok((result_term, result_n))
}

const MAX_REDUCTION_ITERATIONS: usize = 10000;

pub fn repeatedly_reduce_term(
    symbol_table: &HashMap<String, Rc<Term>>,
    term_rc: Rc<Term>,
    bound_variable_opt: &Option<(usize, Rc<Term>)>,
) -> Result<Rc<Term>, String> {
    let mut term = term_rc;
    let mut i: usize = 0;
    loop {
        loop {
            // println!("Reduce iteration #{}", i);
            i += 1;
            if i >= MAX_REDUCTION_ITERATIONS {
                return Err("MAX_REDUCTION_ITERATIONS reached".to_owned());
            }

            let (result_term, substitution_n) =
                reduce_term(symbol_table, term.clone(), bound_variable_opt, false)?;

            if cfg!(feature = "debug") {
                println!("i = {}:\n{}\n", i, result_term);
            }

            if substitution_n > 0 {
                term = result_term;
            } else {
                break;
            }
        }

        // Once resolve_lazy = false has stabilized, do one round of resolve_lazy = true
        // println!("Reduce iteration #{}", i);
        i += 1;
        if i >= MAX_REDUCTION_ITERATIONS {
            return Err("MAX_REDUCTION_ITERATIONS reached".to_owned());
        }

        let (result_term, substitution_n) =
            reduce_term(symbol_table, term.clone(), bound_variable_opt, true)?;

        if cfg!(feature = "debug") {
            println!("i = {}:\n{}\n", i, result_term);
        }

        if substitution_n > 0 {
            term = result_term;
        } else {
            break;
        }
    }
    Ok(term)
}

fn process_expr_inner_unary(
    symbol_table: &HashMap<String, Rc<Term>>,
    inner: &ast::ExpressionInner,
    bound_symbols: &[(ast::Symbol, usize)],
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

            if let Some(bound_symbol) = bound_symbol_opt {
                return Rc::new(Term::Variable(bound_symbol.1));
            }

            if let Some(table_lookup_value) = symbol_table.get(value) {
                return Rc::clone(table_lookup_value);
            }

            if let Some(builtin_value) = builtins::try_ast_symbol_to_builtin_term(value) {
                return Rc::new(builtin_value);
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
    bound_symbols: &[(ast::Symbol, usize)],
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
            } else if let Some(builtin) = builtins::try_ast_symbol_to_builtin_term(value) {
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
    bound_symbols: &[(ast::Symbol, usize)],
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

pub type ProcessResult = Result<(Vec<Rc<Term>>, HashMap<String, Rc<Term>>), String>;

pub fn process(
    program: &[ast::Statement],
    initial_symbol_table: Option<&mut HashMap<String, Rc<Term>>>,
) -> ProcessResult {
    let empty_symbol_table = &mut HashMap::new();
    let symbol_table: &mut HashMap<String, Rc<Term>> =
        initial_symbol_table.unwrap_or(empty_symbol_table);

    let mut output_terms: Vec<Rc<Term>> = vec![];

    for (index, statement) in program.iter().enumerate() {
        match statement {
            ast::Statement::Definition {
                symbol,
                parameters,
                expression,
            } => {
                if symbol_table.contains_key(symbol) {
                    return Err(format!("[runtime] symbol already defined: {}", symbol));
                }
                let bound_params: Vec<(ast::Symbol, usize)> = parameters
                    .iter()
                    .map(|param| (param.clone(), advance_v()))
                    .collect();

                let mut term = process_expr(symbol_table, expression, &bound_params);

                bound_params.iter().rev().for_each(|(_, v)| {
                    term = Rc::new(Term::Abstraction(*v, Rc::clone(&term)));
                });

                symbol_table.insert(symbol.clone(), term);
            }
            ast::Statement::Expression(expression) => {
                let term = process_expr(symbol_table, expression, &[]);
                let result_term = repeatedly_reduce_term(symbol_table, term, &None)?;

                println!("[{}]: {}", index, result_term);

                match &*result_term {
                    Term::Command(_, _) => builtins::perform_command(&result_term),
                    _ => Ok(()),
                }?;

                output_terms.push(result_term);
            }
        }
    }

    Ok((output_terms, symbol_table.clone()))
}
