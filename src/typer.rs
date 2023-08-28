use crate::{
    ast::{TypedExpr, UntypedExpr, Literal, Id},
    types::{Type, Qual, Pred, Kind, TyVar, prelude::t_string, QualType},
};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {}

pub struct Typer {
    type_env: TypeEnv,
}

impl Typer {
    pub fn new(prelude: Prelude) -> Self {
        Self {
            type_env: TypeEnv::new(prelude.0),
        }
    }
}

// TODO: add assumptions
pub struct Prelude(pub TypeClassEnv);

pub struct TypeClassEnv;

struct TypeEnv {
    tyvar_state: TyVarState,
    type_class_env: TypeClassEnv,
}

impl TypeEnv {
    fn new(class_env: TypeClassEnv) -> Self {
        Self {
            tyvar_state: TyVarState,
            type_class_env: TypeClassEnv,
        }
    }
}

struct TyVarState;

struct Constraint(Type, Type);

// TODO: maybe remove `pub`
struct Subst(pub Vec<(TyVar, Type)>);

impl Typer {
    pub fn type_check(&mut self, expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
        let (typed_expr, constraints) = self.infer(expr);
        let _subst = self.unify(constraints);
        // TODO: apply substitutions to typed_expr
        Ok(typed_expr)
    }

    // TODO: remove
    // update TypedExpr to be something that can have TGen values
    // in the middle of the expression (look at the definition of thio::quantify)
    fn infer(&mut self, expr: UntypedExpr) -> (TypedExpr, Vec<Constraint>) {
        let typed_expr = self.infer_expr(expr).expect("type inference failed");
        (typed_expr, vec![])
    }

    fn infer_expr(&mut self, expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
        match expr {
            UntypedExpr::Var(id) => Ok(TypedExpr::Var(id)),
            UntypedExpr::Lit(lit, ()) => Ok(infer_lit(lit)),
            UntypedExpr::App(_, _) => todo!(),
            UntypedExpr::Let(_, _, _, _) => todo!(),
            UntypedExpr::Lambda(_, _, _) => todo!(),
        }
    }


    fn unify(&mut self, _constraints: Vec<Constraint>) -> Subst {
        Subst(vec![])
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
        let mut typer = Typer::new(default_prelude());
        typer.type_check(expr)
    }

    fn default_prelude() -> Prelude {
        Prelude(TypeClassEnv)
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
