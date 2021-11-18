#[macro_use]
extern crate pest_derive;
extern crate from_pest;
#[macro_use]
extern crate pest_ast;
extern crate pest;

mod parser {
    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct Parser;
}

// mod ast {
//     use super::parser::Rule;
//     use pest::Span;

//     fn span_into_str(span: Span) -> &str {
//         span.as_str()
//     }

//     #[derive(Debug, FromPest)]
//     #[pest_ast(rule(Rule::program))]
//     pub struct Program {
//         pub statements: Vec<Statement>,
//         pub eoi: EOI,
//     }

//     #[derive(Debug, FromPest)]
//     #[pest_ast(rule(Rule::statement))]
//     pub enum Statement {
//         Definition(Definition),
//         Expression(Expression),
//     }

//     #[derive(Debug, FromPest)]
//     #[pest_ast(rule(Rule::definition))]
//     pub struct Definition {
//         pub symbols: Vec<Symbol>,
//         pub expression: Expression,
//     }

//     #[derive(Debug, FromPest, Clone)]
//     #[pest_ast(rule(Rule::expression))]
//     pub enum Expression {
//         IntegerLiteral(IntegerLiteral),
//         Symbol(Symbol),
//         SubExpressions(Vec<Expression>),
//     }

//     #[derive(Debug, FromPest, Copy, Clone)]
//     #[pest_ast(rule(Rule::integer_literal))]
//     pub struct IntegerLiteral {
//         #[pest_ast(outer(with(span_into_str), with(str::parse::<i64>), with(Result::unwrap)))]
//         pub value: i64,
//     }

//     #[derive(Debug, FromPest, Clone)]
//     #[pest_ast(rule(Rule::symbol))]
//     pub struct Symbol {
//         #[pest_ast(outer(with(span_into_str), with(String::from)))]
//         pub value: String,
//     }

//     #[derive(Debug, FromPest, Copy, Clone)]
//     #[pest_ast(rule(Rule::EOI))]
//     pub struct EOI;
// }

// mod ir {
//     use super::ast;

//     pub fn left_associate_exprs(program: &ast::Program) -> ast::Program {
//         fn associate(expr: &ast::Expression) -> ast::Expression {
//             match expr.nodes.len() {
//                 1 | 2 => expr.clone(),
//                 3.. => {
//                     let inner_node = ast::ExpressionNode::SubNodes(expr.nodes[0..2].to_vec());
//                     let rest = expr.nodes[2..].to_vec();
//                     let mut new_nodes = vec![inner_node];
//                     new_nodes.extend(rest);
//                     let outer_node = ast::Expression { nodes: new_nodes };
//                     associate(&outer_node)
//                 }
//                 _ => unreachable!(),
//             }
//         }

//         let statements = program
//             .statements
//             .iter()
//             .map(|s| match s {
//                 ast::Statement::Expression(expr) => ast::Statement::Expression(associate(expr)),
//                 ast::Statement::Definition(def) => ast::Statement::Definition(ast::Definition {
//                     symbols: def.symbols.clone(),
//                     expression: associate(&def.expression),
//                 }),
//             })
//             .collect();

//         ast::Program {
//             statements,
//             eoi: program.eoi.clone(),
//         }
//     }
// }

// mod runtime {
//     use super::ast;
//     use std::collections::HashMap;
//     use std::rc::Rc;

//     #[derive(Debug)]
//     pub enum BuiltinFn {
//         Const(i64),
//     }

//     #[derive(Debug)]
//     pub enum Function {
//         Builtin(BuiltinFn),
//     }

//     pub fn evaluate(ast: &ast::Program) {
//         let mut symbol_table: HashMap<String, Rc<Function>> = HashMap::new();

//         // symbol_table.insert(String::from("const"));

//         // fn apply_function(function: &Function, arg: &Function) -> Function {
//         //     match function {
//         //         Function::Builtin(builtin_fn) => match builtin_fn {
//         //             BuiltinFn::Const(value) => Function::Builtin(BuiltinFn::Const(*value)),
//         //         },
//         //     }
//         // }

//         fn evaluate_nonary(
//             table: &HashMap<String, Rc<Function>>,
//             lhs: &ast::ExpressionNode,
//         ) -> Rc<Function> {
//             match lhs {
//                 ast::ExpressionNode::IntegerLiteral(literal) => {
//                     Rc::new(Function::Builtin(BuiltinFn::Const(literal.value)))
//                 }
//                 ast::ExpressionNode::Symbol(symbol) => match symbol.value.as_str() {
//                     "math-zero" => Rc::new(Function::Builtin(BuiltinFn::Const(0))),
//                     sym => (*table.get(sym).expect("symbol not found")).clone(),
//                 },
//                 _ => todo!("_ case in evaluate_nonary"),
//             }
//         }

//         fn evaluate_unary(
//             table: &HashMap<String, Rc<Function>>,
//             lhs: &ast::ExpressionNode,
//             rhs: &ast::ExpressionNode,
//         ) -> Rc<Function> {
//             let rhs_evaled = evaluate_nonary(&table, rhs);
//             todo!("evaluate_unary")
//         }

//         for statement in &ast.statements {
//             let res: Rc<Function> = match statement {
//                 ast::Statement::Expression(expression) => match expression.nodes.len() {
//                     1 => evaluate_nonary(&symbol_table, &expression.nodes[0]),
//                     2 => evaluate_unary(&symbol_table, &expression.nodes[0], &expression.nodes[1]),
//                     _ => unreachable!("expr arity > 2"),
//                 },
//                 ast::Statement::Definition(_) => {
//                     panic!("unsupported statement type: definition")
//                 }
//             };

//             println!("Result: {:#?}", res)
//         }
//     }
// }

fn main() {
    // use ast::Program;
    use from_pest::FromPest;
    use pest::Parser;
    use std::fs;

    let unparsed_file = fs::read_to_string("samples/sample1.code").expect("cannot read file");
    let mut parse_tree =
        parser::Parser::parse(parser::Rule::program, &unparsed_file).expect("unsuccessful parse");

    println!("parse tree = {:#?}", parse_tree);
    // let syntax_tree: Program = Program::from_pest(&mut parse_tree).expect("infallible");
    // println!("syntax tree = {:#?}", syntax_tree);

    // let ir_tree = ir::left_associate_exprs(&syntax_tree);
    // println!("ir tree = {:#?}", ir_tree);

    // runtime::evaluate(&ir_tree);

    // let tokens = program.

    // for token in program.tokens() {
    //     println!("{:?}", token);
    // }

    // println!("{}", program)

    // for statement in program.into_inner() {
    //     match statement.as_rule() {
    //         Rule::statement => {
    //             println!("{}", statement.as_str());
    //         }
    //         Rule::EOI => (),
    //         _ => unreachable!(),
    //     }
    // }

    // println!("Sum of fields: {}", field_sum);
    // println!("Number of records: {}", record_count);
}
