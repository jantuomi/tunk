use super::*;
use runtime::*;
use serial_test::serial;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::Ordering;

fn evaluate_from_source(
    content: String,
    initial_symbol_table: Option<HashMap<String, Rc<Term>>>,
) -> ProcessResult {
    use ast::Program;
    use pest::Parser;

    let parse_tree_result = lparser::LParser::parse(lparser::Rule::program, &content);

    if parse_tree_result.is_err() {
        return Err(format!("{}", parse_tree_result.unwrap_err()));
    }

    let mut parse_tree = parse_tree_result.unwrap();

    let syntax_tree: Program = ast::from_parse_tree(&mut parse_tree);

    runtime::process(&syntax_tree, initial_symbol_table)
}

fn initialize_before_test() {
    VAR_ID_INC.store(0, Ordering::Relaxed);
}

#[test]
#[serial]
fn test_reduce_integer_primitive() -> Result<(), String> {
    initialize_before_test();
    let term = Term::Primitive(Value::Integer(123));
    let (result_term, _) = reduce_term(&HashMap::new(), Rc::new(term.clone()), &None, false)?;
    assert_eq!(*result_term, term);
    Ok(())
}

#[test]
#[serial]
fn test_reduce_string_primitive() -> Result<(), String> {
    initialize_before_test();
    let term = Term::Primitive(Value::String(String::from("foobar")));
    let (result_term, _) = reduce_term(&HashMap::new(), Rc::new(term.clone()), &None, false)?;
    assert_eq!(*result_term, term);
    Ok(())
}

#[test]
#[serial]
fn test_reduce_id_call() -> Result<(), String> {
    initialize_before_test();
    let source = "
    id 1;
    ";
    let (terms, symbol_table) = evaluate_from_source(String::from(source), None)?;
    assert_eq!(terms.len(), 1);

    let term = &terms[0];
    let (result_term, _) = reduce_term(&symbol_table, Rc::clone(term), &None, false)?;

    let expected = Term::Primitive(Value::Integer(1));
    assert_eq!(*result_term, expected);
    Ok(())
}

#[test]
#[serial]
fn test_reduce_id_call_negative() -> Result<(), String> {
    initialize_before_test();
    let source = "
    id 1;
    ";
    let (terms, symbol_table) = evaluate_from_source(String::from(source), None)?;
    assert_eq!(terms.len(), 1);

    let term = &terms[0];
    let (result_term, _) = reduce_term(&symbol_table, Rc::clone(term), &None, false)?;

    let expected = Term::Primitive(Value::Integer(2));
    assert_ne!(*result_term, expected);
    Ok(())
}

#[test]
#[serial]
fn test_define_parameterized_func() -> Result<(), String> {
    initialize_before_test();
    let source = "
    f a b = b a;
    ";
    let (terms, symbol_table) = evaluate_from_source(String::from(source), None)?;
    assert_eq!(terms.len(), 0);

    let term = symbol_table.get("f").ok_or("f not in symbol table")?;

    let (result_term, _) = reduce_term(&symbol_table, Rc::clone(term), &None, false)?;

    let expected = Term::Abstraction(
        0,
        Rc::new(Term::Abstraction(
            1,
            Rc::new(Term::Application(
                Rc::new(Term::Variable(1)),
                Rc::new(Term::Variable(0)),
            )),
        )),
    );
    assert_eq!(*result_term, expected);
    Ok(())
}

#[test]
#[serial]
fn test_reduce_parameterized_func() -> Result<(), String> {
    initialize_before_test();
    let source = "
    f 10 id;
    ";

    let predefined_f = Rc::new(Term::Abstraction(
        0,
        Rc::new(Term::Abstraction(
            1,
            Rc::new(Term::Application(
                Rc::new(Term::Variable(1)),
                Rc::new(Term::Variable(0)),
            )),
        )),
    ));

    let mut initial_symbol_table: HashMap<String, Rc<Term>> = HashMap::new();
    initial_symbol_table.insert(String::from("f"), Rc::clone(&predefined_f));

    let (terms, symbol_table) =
        evaluate_from_source(String::from(source), Some(initial_symbol_table))?;
    assert_eq!(terms.len(), 1);
    let term = &terms[0];

    let (result_term, _) = repeatedly_reduce_term(&symbol_table, Rc::clone(term), &None, false)?;

    let expected = Term::Primitive(Value::Integer(10));
    assert_eq!(*result_term, expected);
    Ok(())
}
