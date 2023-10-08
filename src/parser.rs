use crate::{ast, lexer, types};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxError(lalrpop_util::ParseError<usize, lexer::Token, lexer::LexicalError>);

pub fn parse_expr(input: &str) -> Result<Box<ast::ParsedExpr>, SyntaxError> {
    let tokens = lexer::Lexer::new(input);
    grammar::ExprParser::new()
        .parse(tokens)
        .map_err(SyntaxError)
}

pub fn parse_type_expr(input: &str) -> Result<types::Type, SyntaxError> {
    let tokens = lexer::Lexer::new(input);
    grammar::TypeExprParser::new()
        .parse(tokens)
        .map_err(SyntaxError)
}

pub fn parse_qual_type_expr(input: &str) -> Result<types::QualType, SyntaxError> {
    let tokens = lexer::Lexer::new(input);
    grammar::QualTypeExprParser::new()
        .parse(tokens)
        .map_err(SyntaxError)
}

#[cfg(test)]
mod expr_tests {
    use crate::ast::{Id, Literal, ParsedExpr};

    fn parse(input: &str) -> ParsedExpr {
        *super::parse_expr(input).expect("parsing failed")
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
        let expected =
            ParsedExpr::Lambda(vec![Id::new("x")], Box::new(ParsedExpr::Var(Id::new("x"))));

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

#[cfg(test)]
mod type_expr_tests {
    use crate::{ast::Id, types::{Type, QualType, Kind, TyCon, TyVar}};

    fn parse_type_expr(input: &str) -> Type {
        super::parse_type_expr(input).expect("parsing failed")
    }

    fn parse_qual_type_expr(input: &str) -> QualType {
        super::parse_qual_type_expr(input).expect("parsing failed")
    }

    #[test]
    fn test_type_expr() {
        assert_eq!(
            parse_type_expr("Int"),
            Type::Con(TyCon(Id::new("Int"), Kind::Star))
        );
        assert_eq!(
            parse_type_expr("a"),
            Type::Var(TyVar(Id::new("a"), Kind::Star))
        );
        assert_eq!(
            parse_type_expr("a -> b"),
            Type::Arrow(
                Box::new(Type::Var(TyVar(Id::new("a"), Kind::Star))),
                Box::new(Type::Var(TyVar(Id::new("b"), Kind::Star))),
            )
        );
        assert_eq!(
            parse_type_expr("List Int"),
            Type::App(
                // TODO: should be * -> * actually, not sure how to enforce this on the parser level
                Box::new(Type::Con(TyCon(Id::new("List"), Kind::Star))),
                Box::new(Type::Var(TyVar(Id::new("Int"), Kind::Star))),
            )
        );
    }
}
