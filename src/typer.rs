use std::collections::HashMap;

use crate::{
    ast::{Id, Literal, TypedExpr, UntypedExpr},
    types::{
        prelude::t_string, HasFreeTypeVariables, HasKind, Kind, KindError, Pred, Qual, QualType,
        Scheme, Subst, Substitutes, TGen, TyVar, Type,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {
    UnboundVariable(Id),
    KindError(KindError),
    UnificationError(Type, Type),
    OccursCheckFails(TyVar, Type),
}

pub struct Typer {
    // TODO: probably should be a Map<TGen, QualType>
    var_env: HashMap<Id, Scheme>,
    gen_state: TGenState,
    type_class_env: TypeClassEnv,
}

impl Typer {
    pub fn new(prelude: Prelude) -> Self {
        let initial_var_env = prelude
            .1
            .into_iter()
            .map(|Assumption(id, scheme)| (id, scheme))
            .collect::<HashMap<Id, Scheme>>();

        Self {
            var_env: initial_var_env,
            type_class_env: prelude.0,
            gen_state: TGenState::initial_state(),
        }
    }
}

pub struct Prelude(pub TypeClassEnv, pub Vec<Assumption>);

pub struct TypeClassEnv;

pub struct Assumption(pub Id, pub Scheme);

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

impl Constraint {
    fn new(lhs: Type, rhs: Type) -> Self {
        Self(lhs, rhs)
    }
}

impl Typer {
    pub fn type_check(&mut self, expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
        let (typed_expr, constraints) = self.infer(expr)?;
        let subst = self.unify(constraints)?;
        Ok(typed_expr.apply(&subst))
    }

    // update TypedExpr to be something that can have TGen values
    // in the middle of the expression (look at the definition of thio::quantify)
    fn infer(&mut self, expr: UntypedExpr) -> Result<(TypedExpr, Vec<Constraint>), TypeError> {
        match expr {
            UntypedExpr::Var(id, _) => {
                let scheme = self
                    .var_env
                    .get(&id)
                    .ok_or_else(|| TypeError::UnboundVariable(id.clone()))?
                    .clone();

                let qual_type = self.instantiate(scheme)?;
                Ok((TypedExpr::Var(id, qual_type), vec![]))
            }
            UntypedExpr::Lit(lit, ()) => Ok(self.infer_lit(lit)),
            UntypedExpr::App(fn_expr, arg_expr, ()) => {
                let (typed_fn_expr, mut fn_constraints) = self.infer(*fn_expr)?;
                let (typed_arg_expr, mut arg_constraints) = self.infer(*arg_expr)?;
                // TODO: I believe the kind here is correct, just double check later
                let tyvar = Type::Var(self.gen_state.gen_fresh(Kind::Star));
                let fn_qual_type = typed_fn_expr.clone().get_type();
                let arg_qual_type = typed_arg_expr.clone().get_type();

                // TODO: double check if we really don't need to have the predicates
                // on the the constraint here
                let app_constraint = Constraint(
                    fn_qual_type.clone().ty(),
                    Type::Arrow(
                        Box::new(arg_qual_type.clone().ty()),
                        Box::new(tyvar.clone()),
                    ),
                );

                let mut constraints = vec![];
                constraints.push(app_constraint);
                constraints.append(&mut fn_constraints);
                constraints.append(&mut arg_constraints);

                let mut preds = fn_qual_type.preds();
                preds.append(&mut arg_qual_type.preds());

                // TODO: again, I believe this is correct (based on THIH), but double check later
                // when we have the constraint solving in place
                let qual_type = QualType::new(preds, tyvar);

                let typed_expr =
                    TypedExpr::App(Box::new(typed_fn_expr), Box::new(typed_arg_expr), qual_type);
                Ok((typed_expr, constraints))
            }
            UntypedExpr::Let(_, _, _, _) => todo!(),
            // TODO: it's still possible that we have bugs here:
            // - we're always creating param_tyvar with kind Star
            // - the param_qual_type is also created without any predicates
            UntypedExpr::Lambda(param, (), body_expr) => {
                let param_tyvar = Type::Var(self.gen_state.gen_fresh(Kind::Star));
                let param_qual_type = QualType::new(vec![], param_tyvar.clone());
                self.var_env
                    .insert(param.clone(), Self::dont_generalize(param_qual_type));

                let (typed_body_expr, constraints) = self.infer(*body_expr)?;
                let body_qual_type = typed_body_expr.clone().get_type();
                let body_ty = body_qual_type.clone().ty();
                let body_preds = body_qual_type.preds();

                let arrow_ty = QualType::new(
                    body_preds,
                    Type::Arrow(Box::new(param_tyvar), Box::new(body_ty)),
                );
                let lambda_typed_expr =
                    TypedExpr::Lambda(param, arrow_ty, Box::new(typed_body_expr));
                Ok((lambda_typed_expr, constraints))
            }
        }
    }

    fn infer_lit(&mut self, lit: Literal) -> (TypedExpr, Vec<Constraint>) {
        match lit {
            Literal::Int(_) => {
                let tvar = Type::Var(self.gen_state.gen_fresh(Kind::Star));
                let pred = Pred::new(Id::new("Num"), tvar.clone());
                let expr = TypedExpr::Lit(lit, Qual::new(vec![pred], tvar));
                (expr, vec![])
            }
            Literal::Str(_) => (
                TypedExpr::Lit(lit, QualType::new(vec![], t_string())),
                vec![],
            ),
        }
    }

    /// don't generalize a type, just return it as a scheme
    fn dont_generalize(qual_type: QualType) -> Scheme {
        Scheme(vec![], qual_type)
    }

    /// instantiates all quantified variables in a scheme with fresh type variables
    fn instantiate(&mut self, scheme: Scheme) -> Result<QualType, TypeError> {
        let Scheme(vars, ty) = scheme;

        let substitutions = vars
            .into_iter()
            .map(|var| {
                let fresh = self.gen_state.gen_fresh(var.kind()?);
                Ok((var, Type::Var(fresh)))
            })
            .collect::<Result<Vec<_>, KindError>>()
            .map(Subst)
            .map_err(TypeError::KindError)?;

        Ok(ty.apply(&substitutions))
    }

    fn unify(&mut self, constraints: Vec<Constraint>) -> Result<Subst, TypeError> {
        let substs = constraints
            .into_iter()
            .map(|constr| Unifier::mgu(constr))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Subst::merge(substs))
    }
}

struct Unifier;

impl Unifier {
    pub fn mgu(constr: Constraint) -> Result<Subst, TypeError> {
        match constr {
            Constraint(Type::App(l1, r1), Type::App(l2, r2)) => {
                let s1 = Self::mgu(Constraint(*l1, *l2))?;
                let s2 = Self::mgu(Constraint(*r1, *r2))?;
                Ok(s1.compose(s2))
            }
            Constraint(Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
                let s1 = Self::mgu(Constraint(*l1, *l2))?;
                let s2 = Self::mgu(Constraint(*r1, *r2))?;
                Ok(s1.compose(s2))
            }
            Constraint(Type::Var(tyvar), ty) | Constraint(ty, Type::Var(tyvar)) => {
                Self::var_bind(tyvar, ty)
            }
            Constraint(Type::Con(tycon1), Type::Con(tycon2)) if tycon1 == tycon2 => {
                Ok(Subst::null())
            }
            Constraint(t1, t2) => Err(TypeError::UnificationError(t1, t2)),
        }
    }

    fn var_bind(tyvar: TyVar, ty: Type) -> Result<Subst, TypeError> {
        match ty {
            ty if ty == (Type::Var(tyvar.clone())) => Ok(Subst::null()),
            ty if ty.clone().ftv().contains(&tyvar) => Err(TypeError::OccursCheckFails(tyvar, ty)),
            ty if ty.kind() != tyvar.kind() => Err(TypeError::KindError(KindError::KindMismatch)),

            _ => Ok(Subst::bind(tyvar, ty)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Expr, parser, simplifier, types::TyCon};

    fn infer(input: &str) -> Result<TypedExpr, TypeError> {
        let parsed = parser::parse(input).expect("parsing failed");
        let expr = simplifier::simplify(*parsed).expect("simplification failed");
        let mut typer = Typer::new(default_prelude());
        typer.type_check(expr)
    }

    // TODO: refactor this
    fn default_prelude() -> Prelude {
        // show : forall a . Show a => String
        let show_assump = Assumption(
            Id::new("show"),
            Scheme(
                vec![TyVar(Id::new("a"), Kind::Star)],
                QualType::new(
                    vec![Pred::new(
                        Id::new("Show"),
                        Type::Var(TyVar(Id::new("a"), Kind::Star)),
                    )],
                    Type::Var(TyVar(Id::new("a"), Kind::Star)),
                ),
            ),
        );

        // increment : Int -> Int
        let increment_assump = Assumption(
            Id::new("increment"),
            Scheme(
                vec![],
                QualType::new(
                    vec![],
                    Type::Arrow(
                        Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
                        Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
                    ),
                ),
            ),
        );

        Prelude(TypeClassEnv, vec![show_assump, increment_assump])
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

    #[test]
    fn test_lambda_with_application() -> Result<(), TypeError> {
        let typed_expr = infer("fun x -> increment x")?.stringify_types();
        assert_eq!(
            typed_expr,
            Expr::Lambda(
                Id::new("x"),
                "Int -> Int".into(),
                Box::new(Expr::App(
                    Box::new(Expr::Var(Id::new("increment"), "Int -> Int".into())),
                    Box::new(Expr::Var(Id::new("x"), "Int".into())),
                    "Int".into(),
                ))
            )
        );

        Ok(())
    }

    // TODO: test
    //  fun x -> show x `yields` Show t0 => t0 -> String
    // x -> y -> y + y `yields` Num t1 => t0 -> t1 -> t1
}
