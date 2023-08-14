use crate::ast::{ParsedExpr, UntypedExpr};

#[derive(Debug, PartialEq, Clone)]
pub enum SimplifierError {}

pub fn simplify(parsed_expr: ParsedExpr) -> Result<UntypedExpr, SimplifierError> {
    match parsed_expr {
        ParsedExpr::Var(id) => Ok(UntypedExpr::Var(id)),
        ParsedExpr::Lit(lit) => Ok(UntypedExpr::Lit(lit)),
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
            vec![Id("a".to_owned()), Id("b".to_owned()), Id("c".to_owned())],
            Box::new(ParsedExpr::Var(Id("a".to_owned()))),
        );

        // fun a -> fun b -> a
        let expected = UntypedExpr::Lambda(
            Id("a".to_owned()),
            (),
            Box::new(UntypedExpr::Lambda(
                Id("b".to_owned()),
                (),
                Box::new(UntypedExpr::Lambda(
                    Id("c".to_owned()),
                    (),
                    Box::new(UntypedExpr::Var(Id("a".to_owned()))),
                )),
            )),
        );

        assert_eq!(simplify(parsed_expr), Ok(expected));
    }

    #[test]
    fn test_app() {
        // f a b c
        let parsed_expr = ParsedExpr::App(
            Box::new(ParsedExpr::Var(Id("f".to_owned()))),
            vec![
                Box::new(ParsedExpr::Var(Id("a".to_owned()))),
                Box::new(ParsedExpr::Var(Id("b".to_owned()))),
                Box::new(ParsedExpr::Var(Id("c".to_owned()))),
            ],
        );

        // (((f a) b) c)
        let expected = UntypedExpr::App(
            Box::new(UntypedExpr::App(
                Box::new(UntypedExpr::App(
                    Box::new(UntypedExpr::Var(Id("f".to_owned()))),
                    Box::new(UntypedExpr::Var(Id("a".to_owned()))),
                )),
                Box::new(UntypedExpr::Var(Id("b".to_owned()))),
            )),
            Box::new(UntypedExpr::Var(Id("c".to_owned()))),
        );

        assert_eq!(simplify(parsed_expr), Ok(expected));
    }

    #[test]
    fn complex_expr() {
        // let f = fun a b -> a in f 1 2
        let parsed_expr = ParsedExpr::Let(
            Id("f".to_owned()),
            Box::new(ParsedExpr::Lambda(
                vec![Id("a".to_owned()), Id("b".to_owned())],
                Box::new(ParsedExpr::Var(Id("a".to_owned()))),
            )),
            Box::new(ParsedExpr::App(
                Box::new(ParsedExpr::App(
                    Box::new(ParsedExpr::Var(Id("f".to_owned()))),
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
            Id("f".to_owned()),
            (),
            Box::new(UntypedExpr::Lambda(
                Id("a".to_owned()),
                (),
                Box::new(UntypedExpr::Lambda(
                    Id("b".to_owned()),
                    (),
                    Box::new(UntypedExpr::Var(Id("a".to_owned()))),
                )),
            )),
            Box::new(UntypedExpr::App(
                Box::new(UntypedExpr::App(
                    Box::new(UntypedExpr::Var(Id("f".to_owned()))),
                    Box::new(UntypedExpr::Lit(Literal::Int(1))),
                )),
                Box::new(UntypedExpr::Lit(Literal::Int(2))),
            )),
        );
        assert_eq!(simplify(parsed_expr), Ok(expected));
    }
}
