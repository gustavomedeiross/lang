use lalrpop_util::lalrpop_mod;
use crate::{
    ast::ParsedExpr,
    lexer::{self, Lexer}
};

lalrpop_mod!(pub grammar);

#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxError(
    lalrpop_util::ParseError<usize, lexer::Token, lexer::LexicalError>,
);

pub fn parse(input: &str) -> Result<Box<ParsedExpr>, SyntaxError> {
    let tokens = Lexer::new(input);
    grammar::ExprParser::new()
        .parse(tokens)
        .map_err(SyntaxError)
}

#[cfg(test)]
mod parser_tests {
    use crate::ast::{Id, Literal, ParsedExpr};

    fn parse(input: &str) -> ParsedExpr {
        *super::parse(input).expect("parsing failed")
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

    #[test]
    fn test_subexpr() {
        let result = parse("f (g x)");
        let expected = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id("f".to_owned()))),
            vec![Box::new(ParsedExpr::App(
                Box::new(ParsedExpr::Var(Id("g".to_owned()))),
                vec![Box::new(ParsedExpr::Var(Id("x".to_owned())))],
            ))],
        );

        assert_eq!(result, expected);
    }
}
