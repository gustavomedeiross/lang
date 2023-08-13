use crate::ast::{ParsedExpr, Expr};

#[derive(Debug, PartialEq, Clone)]
pub enum SimplifierError {}

pub fn simplify(parsed_expr: ParsedExpr) -> Result<Expr<()>, SimplifierError> {
// pub fn simplify(parsed_expr: ParsedExpr) -> Expr<()> {
    match parsed_expr {
        ParsedExpr::Var(id) =>  Ok(Expr::Var(id)),
        ParsedExpr::Lit(lit) => Ok(Expr::Lit(lit)),
        // TODO: do something
        // ParsedExpr::App(e1, e2) => Ok(Expr::App(
        //     // TODO: Box.map
        //     Box::new(simplify(*e1)?),
        //     Box::new(simplify(*e2)?),
        // )),
        ParsedExpr::App(_, _) => todo!(),
        ParsedExpr::Let(id, e1, e2) => Ok(Expr::Let(
            id,
            (),
            Box::new(simplify(*e1)?),
            Box::new(simplify(*e2)?),
        )),
        ParsedExpr::Lambda(args, expr) => {
            Ok(args.into_iter().rfold(simplify(*expr)?, |acc, arg| {
                Expr::Lambda(arg, (), Box::new(acc))
            }))
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, ParsedExpr, Id};

    #[test]
    fn test_simplify_lambda() {
        // fun a b -> a
        let parsed_expr = ParsedExpr::Lambda(
            vec![Id("a".to_owned()), Id("b".to_owned())],
            Box::new(ParsedExpr::Var(Id("a".to_owned()))),
        );

        // fun a -> fun b -> a
        let expected = Expr::Lambda(
            Id("a".to_owned()),
            (),
            Box::new(Expr::Lambda(
                Id("b".to_owned()),
                (),
                Box::new(Expr::Var(Id("a".to_owned()))),
            )),
        );

        assert_eq!(simplify(parsed_expr), Ok(expected));
    }
}
