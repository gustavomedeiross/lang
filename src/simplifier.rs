use crate::ast::{ParsedExpr, UntypedExpr};

#[derive(Debug, PartialEq, Clone)]
pub enum SimplifierError {}

pub fn simplify(parsed_expr: ParsedExpr) -> Result<UntypedExpr, SimplifierError> {
    match parsed_expr {
        ParsedExpr::Var(id) => Ok(UntypedExpr::Var(id, ())),
        ParsedExpr::Lit(lit) => Ok(UntypedExpr::Lit(lit, ())),
        ParsedExpr::App(e, es) => {
            let expr = simplify(*e)?;
            es.into_iter().try_fold(expr, |acc, e| {
                Ok(UntypedExpr::App(Box::new(acc), Box::new(simplify(*e)?)))
            })
        }
        ParsedExpr::Let(id, e1, e2) => Ok(UntypedExpr::Let(
            id,
            (),
            Box::new(simplify(*e1)?),
            Box::new(simplify(*e2)?),
        )),
        ParsedExpr::Lambda(args, expr) => {
            Ok(args.into_iter().rfold(simplify(*expr)?, |acc, arg| {
                UntypedExpr::Lambda(arg, (), Box::new(acc))
            }))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Id, Literal};

    #[test]
    fn test_lambda() {
        // fun a b c -> a
        let parsed_expr = ParsedExpr::Lambda(
            vec![Id::new("a"), Id::new("b"), Id::new("c")],
            Box::new(ParsedExpr::Var(Id::new("a"))),
        );

        // fun a -> fun b -> a
        let expected = UntypedExpr::Lambda(
            Id::new("a"),
            (),
            Box::new(UntypedExpr::Lambda(
                Id::new("b"),
                (),
                Box::new(UntypedExpr::Lambda(
                    Id::new("c"),
                    (),
                    Box::new(UntypedExpr::Var(Id::new("a"), ())),
                )),
            )),
        );

        assert_eq!(simplify(parsed_expr), Ok(expected));
    }

    #[test]
    fn test_app() {
        // f a b c
        let parsed_expr = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id::new("f"))),
            vec![
                Box::new(ParsedExpr::Var(Id::new("a"))),
                Box::new(ParsedExpr::Var(Id::new("b"))),
                Box::new(ParsedExpr::Var(Id::new("c"))),
            ],
        );

        // (((f a) b) c)
        let expected = UntypedExpr::App(
            Box::new(UntypedExpr::App(
                Box::new(UntypedExpr::App(
                    Box::new(UntypedExpr::Var(Id::new("f"), ())),
                    Box::new(UntypedExpr::Var(Id::new("a"), ())),
                )),
                Box::new(UntypedExpr::Var(Id::new("b"), ())),
            )),
            Box::new(UntypedExpr::Var(Id::new("c"), ())),
        );

        assert_eq!(simplify(parsed_expr), Ok(expected));
    }

    #[test]
    fn complex_expr() {
        // let f = fun a b -> a in f 1 2
        let parsed_expr = ParsedExpr::Let(
            Id::new("f"),
            Box::new(ParsedExpr::Lambda(
                vec![Id::new("a"), Id::new("b")],
                Box::new(ParsedExpr::Var(Id::new("a"))),
            )),
            Box::new(ParsedExpr::App(
                Box::new(ParsedExpr::App(
                    Box::new(ParsedExpr::Var(Id::new("f"))),
                    vec![
                        Box::new(ParsedExpr::Lit(Literal::Int(1))),
                        Box::new(ParsedExpr::Lit(Literal::Int(2))),
                    ],
                )),
                vec![],
            )),
        );

        // let f = fun a -> fun b -> a in ((f 1) 2)
        let expected = UntypedExpr::Let(
            Id::new("f"),
            (),
            Box::new(UntypedExpr::Lambda(
                Id::new("a"),
                (),
                Box::new(UntypedExpr::Lambda(
                    Id::new("b"),
                    (),
                    Box::new(UntypedExpr::Var(Id::new("a"), ())),
                )),
            )),
            Box::new(UntypedExpr::App(
                Box::new(UntypedExpr::App(
                    Box::new(UntypedExpr::Var(Id::new("f"), ())),
                    Box::new(UntypedExpr::Lit(Literal::Int(1), ())),
                )),
                Box::new(UntypedExpr::Lit(Literal::Int(2), ())),
            )),
        );
        assert_eq!(simplify(parsed_expr), Ok(expected));
    }
}
