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

pub fn parse_type_scheme(input: &str) -> Result<types::Scheme, SyntaxError> {
    let tokens = lexer::Lexer::new(input);
    grammar::TypeSchemeParser::new()
        .parse(tokens)
        .map_err(SyntaxError)
}

pub mod grammar_support {
    use crate::{
        ast::Id,
        types::{Kind, TyCon, TyVar, Type},
    };

    pub fn make_type_application(t1: &str, t2: &str) -> Type {
        let t1_kind = Kind::KFun(Box::new(Kind::Star), Box::new(Kind::Star));
        let t1 = if is_pascal_case(t1) {
            Type::Con(TyCon(Id::new(t1), t1_kind))
        } else {
            Type::Var(TyVar(Id::new(t1), t1_kind))
        };

        let t2_kind = Kind::Star;
        let t2 = if is_pascal_case(t2) {
            Type::Con(TyCon(Id::new(t2), t2_kind))
        } else {
            Type::Var(TyVar(Id::new(t2), t2_kind))
        };

        Type::App(Box::new(t1), Box::new(t2))
    }

    fn is_pascal_case(s: &str) -> bool {
        s.chars()
            .nth(0)
            .expect("precondition failed")
            .is_uppercase()
    }

    fn is_camel_case(s: &str) -> bool {
        s.chars()
            .nth(0)
            .expect("precondition failed")
            .is_lowercase()
    }
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
    use crate::{
        ast::Id,
        types::{Kind, Pred, QualType, Scheme, TyCon, TyVar, Type},
    };

    fn parse_type_expr(input: &str) -> Type {
        super::parse_type_expr(input).expect("parsing failed")
    }

    fn parse_qual_type_expr(input: &str) -> QualType {
        super::parse_qual_type_expr(input).expect("parsing failed")
    }

    fn parse_type_scheme(input: &str) -> Scheme {
        super::parse_type_scheme(input).expect("parsing failed")
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
                Box::new(Type::Con(TyCon(
                    Id::new("List"),
                    Kind::KFun(Box::new(Kind::Star), Box::new(Kind::Star))
                )),),
                Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
            )
        );
    }

    #[test]
    fn test_qual_type_expr() {
        assert_eq!(
            parse_qual_type_expr("Int"),
            QualType::new(vec![], Type::Con(TyCon(Id::new("Int"), Kind::Star)))
        );
        assert_eq!(
            parse_qual_type_expr("(Show a) => a"),
            QualType::new(
                vec![Pred::new(
                    Id::new("Show"),
                    Type::Var(TyVar(Id::new("a"), Kind::Star))
                )],
                Type::Var(TyVar(Id::new("a"), Kind::Star))
            )
        );
        assert_eq!(
            parse_qual_type_expr("(Show a, Eq a) => a"),
            QualType::new(
                vec![
                    Pred::new(Id::new("Show"), Type::Var(TyVar(Id::new("a"), Kind::Star))),
                    Pred::new(Id::new("Eq"), Type::Var(TyVar(Id::new("a"), Kind::Star)))
                ],
                Type::Var(TyVar(Id::new("a"), Kind::Star))
            )
        );
        assert_eq!(
            parse_qual_type_expr("(Show a, Eq b) => a -> b"),
            QualType::new(
                vec![
                    Pred::new(Id::new("Show"), Type::Var(TyVar(Id::new("a"), Kind::Star))),
                    Pred::new(Id::new("Eq"), Type::Var(TyVar(Id::new("b"), Kind::Star)))
                ],
                Type::Arrow(
                    Box::new(Type::Var(TyVar(Id::new("a"), Kind::Star))),
                    Box::new(Type::Var(TyVar(Id::new("b"), Kind::Star))),
                )
            )
        );
    }

    #[test]
    fn test_type_scheme() {
        assert_eq!(
            parse_type_scheme("a . (Show a) => a"),
            Scheme::new(
                vec![TyVar(Id::new("a"), Kind::Star)],
                QualType::new(
                    vec![Pred::new(
                        Id::new("Show"),
                        Type::Var(TyVar(Id::new("a"), Kind::Star))
                    )],
                    Type::Var(TyVar(Id::new("a"), Kind::Star))
                )
            )
        );
        assert_eq!(
            parse_type_scheme("Int -> Int"),
            Scheme::new(
                vec![],
                QualType::new(
                    vec![],
                    Type::Arrow(
                        Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
                        Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
                    )
                )
            )
        );
        assert_eq!(
            parse_type_scheme("Bool"),
            Scheme::new(
                vec![],
                QualType::new(vec![], Type::Con(TyCon(Id::new("Bool"), Kind::Star)))
            )
        );
    }
}
