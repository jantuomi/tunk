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
        pub leaves: Vec<Leaf>,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::leaf))]
    pub enum Leaf {
        Symbol(Symbol),
        IntegerLiteral(IntegerLiteral),
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
