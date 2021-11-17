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
        pub eoi: EOI,
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

    #[derive(Debug, FromPest, Clone)]
    #[pest_ast(rule(Rule::expression))]
    pub struct Expression {
        pub nodes: Vec<ExpressionNode>,
    }

    #[derive(Debug, FromPest, Clone)]
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

    #[derive(Debug, FromPest, Copy, Clone)]
    #[pest_ast(rule(Rule::integer_literal))]
    pub struct IntegerLiteral {
        #[pest_ast(outer(with(span_into_str), with(str::parse::<i64>), with(Result::unwrap)))]
        pub value: i64,
    }

    #[derive(Debug, FromPest, Clone)]
    #[pest_ast(rule(Rule::symbol))]
    pub struct Symbol {
        #[pest_ast(outer(with(span_into_str), with(String::from)))]
        pub value: String,
    }

    #[derive(Debug, FromPest, Copy, Clone)]
    #[pest_ast(rule(Rule::EOI))]
    pub struct EOI;
}

mod ir {
    use super::ast;

    pub fn left_associate_exprs(program: &ast::Program) -> ast::Program {
        fn associate(expr: &ast::Expression) -> ast::Expression {
            match expr.nodes.len() {
                1 | 2 => expr.clone(),
                3.. => {
                    let inner_node = ast::ExpressionNode::Expression(ast::Expression {
                        nodes: expr.nodes[0..2].to_vec(),
                    });
                    let rest = expr.nodes[2..].to_vec();
                    let mut new_nodes = vec![inner_node];
                    new_nodes.extend(rest);
                    let outer_node = ast::Expression { nodes: new_nodes };
                    associate(&outer_node)
                }
                _ => unreachable!(),
            }
        }

        let statements = program
            .statements
            .iter()
            .map(|s| match s {
                ast::Statement::Expression(expr) => ast::Statement::Expression(associate(expr)),
                ast::Statement::Definition(def) => ast::Statement::Definition(ast::Definition {
                    symbols: def.symbols.clone(),
                    expression: associate(&def.expression),
                }),
            })
            .collect();

        ast::Program {
            statements,
            eoi: program.eoi.clone(),
        }
    }
}

mod runtime {
    use super::ast;
    use std::collections::HashMap;

    #[derive(Debug, Clone)]
    pub enum RuntimeExpression {
        Integer(i64),
        Function {
            unbound: Vec<String>,
            bound: HashMap<String, Box<RuntimeExpression>>,
            body: Box<RuntimeExpression>,
        },
    }

    pub fn evaluate(ast: &ast::Program) {
        let mut symbol_table: HashMap<String, RuntimeExpression> = HashMap::new();
        symbol_table.insert(
            String::from("my-func"),
            RuntimeExpression::Function {
                unbound: vec![String::from("a"), String::from("b")],
                bound: HashMap::new(),
                body: Box::new(RuntimeExpression::Integer(10)),
            },
        );

        fn apply_function(function: &RuntimeExpression) -> Box<RuntimeExpression> {
            match function {
                RuntimeExpression::Function {
                    bound,
                    unbound,
                    body,
                } => todo!("function application"),
                _ => unreachable!("trying to apply something else than a function"),
            }
        }

        fn evaluate_nonary(
            table: &HashMap<String, RuntimeExpression>,
            node: &ast::ExpressionNode,
        ) -> Box<RuntimeExpression> {
            match node {
                ast::ExpressionNode::IntegerLiteral(literal) => {
                    Box::new(RuntimeExpression::Integer(literal.value))
                }
                ast::ExpressionNode::Symbol(symbol) => {
                    let rt_expr = table
                        .get(&symbol.value)
                        .expect(&format!("no such symbol: {}", symbol.value));
                    return Box::new(rt_expr.clone());
                }
                _ => {
                    println!("_ case of evaluate_nonary for {:#?}", node);
                    panic!()
                }
            }
        }

        fn evaluate_unary(
            table: &HashMap<String, RuntimeExpression>,
            node: &ast::ExpressionNode,
            arg: &ast::ExpressionNode,
        ) -> Box<RuntimeExpression> {
            let evaluated_arg = evaluate_expression_node(table, arg);
            match node {
                ast::ExpressionNode::Symbol(symbol) => {
                    let rt_expr = table.get(&symbol.value).expect(&format!("no such symbol"));

                    match rt_expr {
                        RuntimeExpression::Integer(_) => {
                            panic!("trying to apply to integer like a function")
                        }
                        RuntimeExpression::Function {
                            unbound,
                            bound,
                            body,
                        } => {
                            let unbounds_left = unbound.len();
                            if unbounds_left == 0 {
                                unreachable!("function is somehow called with arguments while it has no unbound parameters")
                            }

                            let mut new_bound: HashMap<String, Box<RuntimeExpression>> =
                                HashMap::new();
                            for (key, value) in bound {
                                new_bound.insert(String::from(key), Box::new(*value.clone()));
                            }

                            let next_to_bind = &unbound[0];
                            new_bound.insert(String::from(next_to_bind), evaluated_arg);

                            let new_unbound = unbound[1..].to_vec();
                            let new_body = Box::new(*body.clone());

                            let new_unbound_length = new_unbound.len();

                            let new_func = RuntimeExpression::Function {
                                unbound: new_unbound,
                                bound: new_bound,
                                body: new_body,
                            };

                            if new_unbound_length == 0 {
                                return apply_function(&new_func);
                            } else {
                                return Box::new(new_func);
                            }
                        }
                    }
                }
                _ => {
                    println!("_ case of evaluate_unary for {:#?}, {:#?}", node, arg);
                    panic!()
                }
            }
        }

        fn evaluate_expression(
            table: &HashMap<String, RuntimeExpression>,
            expression: &ast::Expression,
        ) -> Box<RuntimeExpression> {
            let nodes = &expression.nodes;
            return match nodes.len() {
                1 => evaluate_nonary(table, &nodes[0]),
                2 => evaluate_unary(table, &nodes[0], &nodes[1]),
                _ => unreachable!("expression has more than 2 nodes"),
            };
        }

        fn evaluate_expression_node(
            table: &HashMap<String, RuntimeExpression>,
            node: &ast::ExpressionNode,
        ) -> Box<RuntimeExpression> {
            match node {
                ast::ExpressionNode::Expression(expr) => evaluate_expression(table, expr),
                ast::ExpressionNode::IntegerLiteral(_) => evaluate_nonary(&table, node),
                _ => todo!("_ case of evaluate_expression_node"),
            }
        }

        for statement in &ast.statements {
            let runtime_expr = match statement {
                ast::Statement::Expression(expression) => {
                    evaluate_expression(&symbol_table, &expression)
                }
                ast::Statement::Definition(_) => {
                    panic!("unsupported statement type: definition")
                }
            };

            match *runtime_expr {
                RuntimeExpression::Integer(value) => println!("{}", value),
                other => println!("{:#?}", other),
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

    let ir_tree = ir::left_associate_exprs(&syntax_tree);
    println!("ir tree = {:#?}", ir_tree);

    runtime::evaluate(&ir_tree);

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
