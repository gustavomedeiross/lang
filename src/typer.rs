use crate::{
    ast::{TypedExpr, UntypedExpr},
    types::Type,
};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {}

pub fn infer(expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
    match expr {
        UntypedExpr::Var(id) => Ok(TypedExpr::Var(id)),
        UntypedExpr::Lit(lit) => Ok(TypedExpr::Lit(lit)),
        UntypedExpr::App(_, _) => todo!(),
        UntypedExpr::Let(_, _, _, _) => todo!(),
        UntypedExpr::Lambda(_, _, _) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Literal, parser, simplifier};

    fn infer(input: &str) -> Result<TypedExpr, TypeError> {
        let parsed = parser::parse(input).expect("parsing failed");
        let expr = simplifier::simplify(*parsed).expect("simplification failed");
        super::infer(expr)
    }

    #[test]
    fn test_literals() {
        assert_eq!(infer("1"), Ok(TypedExpr::Lit(Literal::Int(1))));
        assert_eq!(
            infer(r#""hello""#),
            Ok(TypedExpr::Lit(Literal::Str(r#""hello""#.to_owned())))
        );
    }
}
