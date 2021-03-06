use super::*;
use runtime::*;
use serial_test::serial;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::Ordering;

fn evaluate_from_source(
    content: String,
    initial_symbol_table: Option<&mut HashMap<String, Rc<Term>>>,
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
fn reduce_integer_primitive() -> Result<(), String> {
    initialize_before_test();
    let term = Term::Primitive(Value::Integer(123));
    let (result_term, _) = reduce_term(&HashMap::new(), Rc::new(term.clone()), &None, false)?;
    assert_eq!(*result_term, term);
    Ok(())
}

#[test]
#[serial]
fn reduce_string_primitive() -> Result<(), String> {
    initialize_before_test();
    let term = Term::Primitive(Value::String("foobar".to_owned()));
    let (result_term, _) = reduce_term(&HashMap::new(), Rc::new(term.clone()), &None, false)?;
    assert_eq!(*result_term, term);
    Ok(())
}

#[test]
#[serial]
fn reduce_id_call() -> Result<(), String> {
    initialize_before_test();
    let source = "
    id 1;
    ";
    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 1);

    let term = &terms[0];
    let (result_term, _) = reduce_term(&symbol_table, Rc::clone(term), &None, false)?;

    let expected = Term::Primitive(Value::Integer(1));
    assert_eq!(*result_term, expected);
    Ok(())
}

#[test]
#[serial]
fn reduce_id_call_negative() -> Result<(), String> {
    initialize_before_test();
    let source = "
    id 1;
    ";
    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 1);

    let term = &terms[0];
    let (result_term, _) = reduce_term(&symbol_table, Rc::clone(term), &None, false)?;

    let expected = Term::Primitive(Value::Integer(2));
    assert_ne!(*result_term, expected);
    Ok(())
}

#[test]
#[serial]
fn define_parameterized_func() -> Result<(), String> {
    initialize_before_test();
    let source = "
    f a b = b a;
    ";
    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
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
fn reduce_parameterized_func() -> Result<(), String> {
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
    initial_symbol_table.insert("f".to_owned(), Rc::clone(&predefined_f));

    let (terms, symbol_table) =
        evaluate_from_source(source.to_owned(), Some(&mut initial_symbol_table))?;
    assert_eq!(terms.len(), 1);
    let term = &terms[0];

    let result_term = repeatedly_reduce_term(&symbol_table, Rc::clone(term), &None)?;

    let expected = Term::Primitive(Value::Integer(10));
    assert_eq!(*result_term, expected);
    Ok(())
}

#[test]
#[serial]
fn reduce_builtin_int_add() -> Result<(), String> {
    initialize_before_test();
    let source = "
    int.add;
    int.add 10;
    int.add 10 20;
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 3);
    let term1 = &terms[0];
    let term2 = &terms[1];
    let term3 = &terms[2];

    // First expression
    let result_term1 = repeatedly_reduce_term(&symbol_table, Rc::clone(term1), &None)?;
    let expected1_identifier = builtins::B_INTEGER_ADD;
    let result_builtin1 = match &*result_term1 {
        Term::Builtin(b) => b,
        _ => return Err(format!("{} is not a builtin", result_term1)),
    };

    assert_eq!(result_builtin1.identifier, expected1_identifier);

    // Second expression
    let result_term2 = repeatedly_reduce_term(&symbol_table, Rc::clone(term2), &None)?;
    let expected2_identifier = builtins::B_INTEGER_ADD;
    let result_builtin2 = match &*result_term2 {
        Term::Builtin(b) => b,
        _ => return Err(format!("{} is not a builtin", result_term2)),
    };

    assert_eq!(result_builtin2.identifier, expected2_identifier);

    // Third expression
    let result_term3 = repeatedly_reduce_term(&symbol_table, Rc::clone(term3), &None)?;
    let expected3 = Value::Integer(30);
    let result_builtin3 = match &*result_term3 {
        Term::Primitive(p) => p,
        _ => return Err(format!("{} is not a primitive", result_term3)),
    };

    assert_eq!(*result_builtin3, expected3);

    Ok(())
}

#[test]
#[serial]
fn reduce_builtin_int_eq() -> Result<(), String> {
    initialize_before_test();
    let source = "
    int.eq? 10 20
        \"true\"
        \"false\";
    int.eq? 30 30
        \"true\"
        \"false\";
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 2);
    let term1 = &terms[0];
    let term2 = &terms[1];

    // First expression
    let result_term1 = repeatedly_reduce_term(&symbol_table, Rc::clone(term1), &None)?;
    let expected1_string = Value::String("false".to_owned());
    let result_builtin1 = match &*result_term1 {
        Term::Primitive(p) => p,
        _ => return Err(format!("{} is not a primitive", result_term1)),
    };

    assert_eq!(*result_builtin1, expected1_string);

    // Second expression
    let result_term2 = repeatedly_reduce_term(&symbol_table, Rc::clone(term2), &None)?;
    let expected2_string = Value::String("true".to_owned());
    let result_builtin2 = match &*result_term2 {
        Term::Primitive(p) => p,
        _ => return Err(format!("{} is not a primitive", result_term2)),
    };

    assert_eq!(*result_builtin2, expected2_string);

    Ok(())
}

#[test]
#[serial]
fn reduce_builtin_str_eq() -> Result<(), String> {
    initialize_before_test();
    let source = "
    string.eq? \"foo\" \"bar\"
        \"true\"
        \"false\";
    string.eq? \"baz\" \"baz\"
        \"true\"
        \"false\";
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 2);
    let term1 = &terms[0];
    let term2 = &terms[1];

    // First expression
    let result_term1 = repeatedly_reduce_term(&symbol_table, Rc::clone(term1), &None)?;
    let expected1_string = Value::String("false".to_owned());
    let result_builtin1 = match &*result_term1 {
        Term::Primitive(p) => p,
        _ => return Err(format!("{} is not a primitive", result_term1)),
    };

    assert_eq!(*result_builtin1, expected1_string);

    // Second expression
    let result_term2 = repeatedly_reduce_term(&symbol_table, Rc::clone(term2), &None)?;
    let expected2_string = Value::String("true".to_owned());
    let result_builtin2 = match &*result_term2 {
        Term::Primitive(p) => p,
        _ => return Err(format!("{} is not a primitive", result_term2)),
    };

    assert_eq!(*result_builtin2, expected2_string);

    Ok(())
}

#[test]
#[serial]
fn reduce_nontrivial_terminates1() -> Result<(), String> {
    initialize_before_test();
    let source = "
    string.eq?
        (bool.to-string
            true);
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 1);
    let term1 = &terms[0];

    let (_, _) = reduce_term(&symbol_table, Rc::clone(term1), &None, false)?;

    // Terminates
    Ok(())
}

#[test]
#[serial]
fn reduce_nontrivial_terminates2() -> Result<(), String> {
    initialize_before_test();
    let source = "
    result =
        bool.to-string
            (int.eq? 2 2);

    string.eq? result \"true\";
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 1);
    let term1 = &terms[0];

    let _ = repeatedly_reduce_term(&symbol_table, Rc::clone(term1), &None)?;

    // Terminates
    Ok(())
}

#[test]
#[serial]
fn problem_factorial() -> Result<(), String> {
    initialize_before_test();
    let source = "
    factorial n =
        int.eq? n 0
            1
            (int.mul n (factorial (int.sub n 1)));

    factorial 20;
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 1);
    let term1 = &terms[0];

    let result_rc = repeatedly_reduce_term(&symbol_table, Rc::clone(term1), &None)?;
    let expected = Term::Primitive(Value::Integer(2432902008176640000));

    assert_eq!(*result_rc, expected);

    Ok(())
}

#[test]
#[serial]
fn problem_triangle_numbers() -> Result<(), String> {
    initialize_before_test();
    let source = "
    triangle n = int.eq? n 1
        1
        (int.add
            n
            (triangle (int.sub n 1)));

    triangle 100;
    ";

    let (terms, symbol_table) = evaluate_from_source(source.to_owned(), None)?;
    assert_eq!(terms.len(), 1);
    let term1 = &terms[0];

    let result_rc = repeatedly_reduce_term(&symbol_table, Rc::clone(term1), &None)?;
    let expected = Term::Primitive(Value::Integer(5050));

    assert_eq!(*result_rc, expected);

    Ok(())
}
