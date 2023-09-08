use std::collections::HashMap;

use crate::{
    ast::{Id, Literal, TypedExpr, UntypedExpr},
    types::{prelude::t_string, Kind, Pred, Qual, QualType, TyVar, Type, TGen},
};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {
    UnboundVariable(Id),
}

pub struct Typer {
    // TODO: probably should be a Map<TGen, QualType>
    // TODO: should be a type scheme
    var_env: HashMap<Id, QualType>,
    gen_state: TGenState,
    type_class_env: TypeClassEnv,
}

impl Typer {
    pub fn new(prelude: Prelude) -> Self {
        Self {
            var_env: HashMap::new(),
            type_class_env: prelude.0,
            gen_state: TGenState::initial_state(),
        }
    }
}

// TODO: add assumptions
pub struct Prelude(pub TypeClassEnv);

pub struct TypeClassEnv;

struct TGenState(TGen);

impl TGenState {
    pub fn initial_state() -> Self {
        Self(TGen::initial())
    }

    pub fn gen_fresh(&mut self, kind: Kind) -> TyVar {
        let curr = self.0.clone();
        self.0 = curr.clone().next();
        curr.to_tyvar(kind)
    }
}

struct Constraint(Type, Type);

// TODO: maybe remove `pub`
struct Subst(pub Vec<(TyVar, Type)>);

impl Typer {
    pub fn type_check(&mut self, expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
        let (typed_expr, constraints) = self.infer(expr)?;
        let _subst = self.unify(constraints);
        // TODO: apply substitutions to typed_expr
        Ok(typed_expr)
    }

    // update TypedExpr to be something that can have TGen values
    // in the middle of the expression (look at the definition of thio::quantify)
    fn infer(&mut self, expr: UntypedExpr) -> Result<(TypedExpr, Vec<Constraint>), TypeError> {
        let typed_expr = self.infer_expr(expr)?;
        Ok((typed_expr, vec![]))
    }

    fn unify(&mut self, _constraints: Vec<Constraint>) -> Subst {
        Subst(vec![])
    }

    fn infer_expr(&mut self, expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
        match expr {
            UntypedExpr::Var(id, _) => {
                let scheme = self.var_env.get(&id)
                    .ok_or_else(|| TypeError::UnboundVariable(id.clone()))?
                    .clone();

                let qual_type = self.instantiate(scheme);
                Ok(TypedExpr::Var(id, qual_type))
            },
            UntypedExpr::Lit(lit, ()) => Ok(self.infer_lit(lit)),
            UntypedExpr::App(_, _) => todo!(),
            UntypedExpr::Let(_, _, _, _) => todo!(),
            UntypedExpr::Lambda(_, _, _) => todo!(),
        }
    }

    fn infer_lit(&mut self, lit: Literal) -> TypedExpr {
        match lit {
            Literal::Int(_) => {
                let tvar = Type::Var(self.gen_state.gen_fresh(Kind::Star));
                let pred = Pred::new(Id::new("Num"), tvar.clone());
                TypedExpr::Lit(lit.clone(), Qual::new(vec![pred], tvar))
            }
            Literal::Str(_) => TypedExpr::Lit(lit, QualType::new(vec![], t_string())),
        }
    }

    fn instantiate(&mut self, scheme: QualType) -> QualType {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Expr, parser, simplifier};

    fn infer(input: &str) -> Result<TypedExpr, TypeError> {
        let parsed = parser::parse(input).expect("parsing failed");
        let expr = simplifier::simplify(*parsed).expect("simplification failed");
        let mut typer = Typer::new(default_prelude());
        typer.type_check(expr)
    }

    fn default_prelude() -> Prelude {
        Prelude(TypeClassEnv)
    }

    #[test]
    fn test_literal_num() -> Result<(), TypeError> {
        let typed_expr = infer("1")?.stringify_types();
        let qual_type = "Num t0 => t0".to_owned();
        assert_eq!(typed_expr, Expr::Lit(Literal::Int(1), qual_type));
        Ok(())
    }

    #[test]
    fn test_literal_string() -> Result<(), TypeError> {
        let typed_expr = infer(r#""hello""#)?.stringify_types();
        let qual_type = "List Char".to_owned();
        assert_eq!(
            typed_expr,
            Expr::Lit(Literal::Str(r#""hello""#.to_owned()), qual_type)
        );
        Ok(())
    }

    #[test]
    fn test_lambda_identity() -> Result<(), TypeError> {
        let typed_expr = infer("fun x -> x")?.stringify_types();
        let qual_type = "t0 -> t0".to_owned();
        assert_eq!(
            typed_expr,
            Expr::Lambda(
                Id::new("x"),
                "t0 -> t0".to_string(),
                Box::new(Expr::Var(Id::new("x"), "t0".to_string()))
            )
        );
        Ok(())
    }
}
