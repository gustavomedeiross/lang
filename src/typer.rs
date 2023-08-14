use crate::ast::{Expr, Id, Literal};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {}

pub fn infer(expr: Expr<()>) -> Result<Expr<Type>, TypeError> {
    match expr {
        Expr::Var(id) => Ok(Expr::Var(id)),
        Expr::Lit(lit) => Ok(Expr::Lit(lit)),
        Expr::App(_, _) => todo!(),
        Expr::Let(_, _, _, _) => todo!(),
        Expr::Lambda(_, _, _) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser, simplifier};

    fn infer(input: &str) -> Result<Expr<Type>, TypeError> {
        let parsed = parser::parse(input).expect("parsing failed");
        let expr = simplifier::simplify(*parsed).expect("simplification failed");
        super::infer(expr)
    }

    #[test]
    fn test_literals() {
        assert_eq!(infer("1"), Ok(Expr::Lit(Literal::Int(1))));
        assert_eq!(
            infer(r#""hello""#),
            Ok(Expr::Lit(Literal::Str(r#""hello""#.to_owned())))
        );
    }
}
