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

mod ast {
    use super::parser::Rule;
    use pest::Span;

    fn span_into_str(span: Span) -> &str {
        span.as_str()
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::program))]
    pub struct Program {
        pub statements: Vec<Statement>,
        eoi: EOI,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::statement))]
    pub enum Statement {
        Definition(Definition),
        Expression(Expression),
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::definition))]
    pub struct Definition {
        pub symbols: Vec<Symbol>,
        pub expression: Expression,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::expression))]
    pub struct Expression {
        pub nodes: Vec<ExpressionNode>,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::expression_node))]
    pub enum ExpressionNode {
        Symbol(Symbol),
        Builtin(Builtin),
        IntegerLiteral(IntegerLiteral),
        Expression(Expression),
    }

    #[derive(Debug, FromPest, Copy, Clone)]
    #[pest_ast(rule(Rule::builtin))]
    pub enum Builtin {
        SumOp,
        SubtractOp,
        DivideOp,
        MultiplyOp,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::integer_literal))]
    pub struct IntegerLiteral {
        #[pest_ast(outer(with(span_into_str), with(str::parse::<i64>), with(Result::unwrap)))]
        pub value: i64,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::symbol))]
    pub struct Symbol {
        #[pest_ast(outer(with(span_into_str), with(String::from)))]
        pub value: String,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::EOI))]
    struct EOI;
}

mod runtime {
    use super::ast;
    use std::collections::HashMap;

    pub enum RuntimeExpression {
        Integer(i64),
        BuiltinFunction(ast::Builtin, Box<RuntimeExpression>),
    }

    pub fn evaluate(ast: &ast::Program) {
        let mut _symbol_table: HashMap<String, RuntimeExpression> = HashMap::new();

        fn evaluate_nonary(node: &ast::ExpressionNode) -> Box<RuntimeExpression> {
            match node {
                ast::ExpressionNode::IntegerLiteral(literal) => {
                    Box::new(RuntimeExpression::Integer(literal.value))
                }
                _ => todo!("_ case of evaluate_nonary"),
            }
        }

        fn evaluate_unary(
            node: &ast::ExpressionNode,
            arg: &ast::ExpressionNode,
        ) -> Box<RuntimeExpression> {
            let evaluated_arg = evaluate_expression_node(arg);
            match node {
                ast::ExpressionNode::Builtin(builtin) => {
                    Box::new(RuntimeExpression::BuiltinFunction(*builtin, evaluated_arg))
                }
                _ => todo!("_ case of evaluate_unary"),
            }
        }

        fn evaluate_expression_node(node: &ast::ExpressionNode) -> Box<RuntimeExpression> {
            match node {
                ast::ExpressionNode::Expression(expr) => evaluate_expression(expr),
                _ => todo!("_ case of evaluate_expression_node"),
            }
        }

        fn evaluate_expression(expression: &ast::Expression) -> Box<RuntimeExpression> {
            let nodes = &expression.nodes;
            return match nodes.len() {
                1 => evaluate_nonary(&nodes[0]),
                2 => evaluate_unary(&nodes[0], &nodes[1]),
                _ => todo!("_ case of evaluate_expression"),
            };
        }

        for statement in &ast.statements {
            let runtime_expr = match statement {
                ast::Statement::Expression(expression) => evaluate_expression(&expression),
                ast::Statement::Definition(_) => {
                    panic!("unsupported statement type: definition")
                }
            };

            match *runtime_expr {
                RuntimeExpression::Integer(value) => println!("{}", value),
                RuntimeExpression::BuiltinFunction(builtin, _arg) => match builtin {
                    _ => todo!("_ case of runtime_expr eval handling"),
                },
            }
        }
    }
}

fn main() {
    use ast::Program;
    use from_pest::FromPest;
    use pest::Parser;
    use std::fs;

    let unparsed_file = fs::read_to_string("samples/sample1.code").expect("cannot read file");
    let mut parse_tree =
        parser::Parser::parse(parser::Rule::program, &unparsed_file).expect("unsuccessful parse");

    println!("parse tree = {:#?}", parse_tree);
    let syntax_tree: Program = Program::from_pest(&mut parse_tree).expect("infallible");
    println!("syntax tree = {:#?}", syntax_tree);

    runtime::evaluate(&syntax_tree);

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
