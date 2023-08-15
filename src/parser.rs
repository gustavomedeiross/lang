use crate::{
    ast::ParsedExpr,
    lexer::{self, Lexer},
};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxError(lalrpop_util::ParseError<usize, lexer::Token, lexer::LexicalError>);

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
            vec![Id::new("x")],
            Box::new(ParsedExpr::Var(Id::new("x"))),
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_application_1() {
        let result = parse("f x");
        let expected = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id::new("f"))),
            vec![Box::new(ParsedExpr::Var(Id::new("x")))],
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_application_2() {
        let result = parse("f x y");
        let expected = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id::new("f"))),
            vec![
                Box::new(ParsedExpr::Var(Id::new("x"))),
                Box::new(ParsedExpr::Var(Id::new("y"))),
            ],
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_let() {
        let result = parse("let x = 1 in x");
        let expected = ParsedExpr::Let(
            Id::new("x"),
            Box::new(ParsedExpr::Lit(Literal::Int(1))),
            Box::new(ParsedExpr::Var(Id::new("x"))),
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_subexpr() {
        let result = parse("f (g x)");
        let expected = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id::new("f"))),
            vec![Box::new(ParsedExpr::App(
                Box::new(ParsedExpr::Var(Id::new("g"))),
                vec![Box::new(ParsedExpr::Var(Id::new("x")))],
            ))],
        );

        assert_eq!(result, expected);
    }
}
