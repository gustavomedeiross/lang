use crate::{
    ast::{TypedExpr, UntypedExpr, Literal, Id},
    types::{Type, Qual, Pred, Kind, TyVar, prelude::t_string, QualType},
};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {}

pub fn infer(expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
    match expr {
        UntypedExpr::Var(id) => Ok(TypedExpr::Var(id)),
        UntypedExpr::Lit(lit, ()) => Ok(infer_lit(lit)),
        UntypedExpr::App(_, _) => todo!(),
        UntypedExpr::Let(_, _, _, _) => todo!(),
        UntypedExpr::Lambda(_, _, _) => todo!(),
    }
}

fn infer_lit(lit: Literal) -> TypedExpr {
    match lit {
        Literal::Int(_) => {
            // TODO: use tyvar generator, this implementation is wrong
            let tvar = Type::Var(TyVar(Id::new("a"), Kind::Star));
            let pred = Pred::new(Id::new("Num"), tvar.clone());
            TypedExpr::Lit(lit.clone(), Qual::new(vec![pred], tvar))
        },
        Literal::Str(_) => {
            TypedExpr::Lit(lit, QualType::new(vec![], t_string()))
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser, simplifier};

    fn infer(input: &str) -> Result<TypedExpr, TypeError> {
        let parsed = parser::parse(input).expect("parsing failed");
        let expr = simplifier::simplify(*parsed).expect("simplification failed");
        super::infer(expr)
    }

    // TODO: make tests nicer to read
    #[test]
    fn test_literals() {
        // Num a => a
        let ty_var = Type::Var(TyVar(Id::new("a"), Kind::Star));
        let pred = Pred::new(Id::new("Num"), ty_var.clone());
        let qual_type = Qual::new(vec![pred], ty_var);
        assert_eq!(infer("1"), Ok(TypedExpr::Lit(Literal::Int(1), qual_type)));

        // String
        let qual_type = Qual::new(vec![], t_string());
        assert_eq!(
            infer(r#""hello""#),
            Ok(TypedExpr::Lit(Literal::Str(r#""hello""#.to_owned()), qual_type))
        );
    }
}
