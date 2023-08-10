mod ast;
mod lexer;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar);

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod parser_tests {
    use super::ast::{Id, Literal, ParsedExpr};
    use super::*;

    fn parse(input: &str) -> ParsedExpr {
        let tokens = lexer::Lexer::new(input);
        let result = grammar::ExprParser::new()
            .parse(tokens)
            .expect("parsing failed");
        *result
    }

    #[test]
    fn test_literals() {
        assert_eq!(parse("1"), ParsedExpr::Lit(Literal::Int(1)));
        assert_eq!(
            parse(r#""hello""#),
            ParsedExpr::Lit(Literal::Str(r#""hello""#.to_owned()))
        );
    }

    #[test]
    fn test_lambda() {
        let result = parse("fun x -> x");
        let expected = ParsedExpr::Lambda(
            vec![Id("x".to_owned())],
            Box::new(ParsedExpr::Var(Id("x".to_owned()))),
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_application_1() {
        let result = parse("f x");
        let expected = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id("f".to_owned()))),
            vec![Box::new(ParsedExpr::Var(Id("x".to_owned())))],
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_application_2() {
        let result = parse("f x y");
        let expected = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id("f".to_owned()))),
            vec![
                Box::new(ParsedExpr::Var(Id("x".to_owned()))),
                Box::new(ParsedExpr::Var(Id("y".to_owned()))),
            ],
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_let() {
        let result = parse("let x = 1 in x");
        let expected = ParsedExpr::Let(
            Id("x".to_owned()),
            Box::new(ParsedExpr::Lit(Literal::Int(1))),
            Box::new(ParsedExpr::Var(Id("x".to_owned()))),
        );

        assert_eq!(result, expected);
    }
}
