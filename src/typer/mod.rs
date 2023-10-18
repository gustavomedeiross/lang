use crate::{
    ast::{Id, Literal, TypedExpr, UntypedExpr},
    types::{
        prelude::t_string, HasFreeTypeVariables, HasKind, Kind, KindError, Pred, Qual, QualType,
        Scheme, Subst, Substitutes, TGen, TyVar, Type,
    },
};

mod constraint;
mod env;
mod unifier;

#[cfg(test)]
mod tests;

use constraint::*;
pub use env::*;
use unifier::*;

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {
    UnboundVariable(Id),
    KindError(KindError),
    UnificationError(Type, Type),
    OccursCheckFails(TyVar, Type),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Type error: ")?;
        match self {
            TypeError::UnboundVariable(id) => write!(f, "unbound variable: {}", id),
            TypeError::KindError(err) => write!(f, "kind error: {}", err),
            TypeError::UnificationError(t1, t2) => {
                write!(f, "unification error: {} != {}", t1, t2)
            }
            TypeError::OccursCheckFails(tyvar, ty) => {
                write!(f, "occurs check fails: {} occurs in {}", tyvar, ty)
            }
        }
    }
}

pub struct Typer {
    gen_state: TGenState,
    type_class_env: TypeClassEnv,
}

impl Typer {
    pub fn new(type_class_env: TypeClassEnv) -> Self {
        Self {
            type_class_env,
            gen_state: TGenState::initial_state(),
        }
    }
}

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

impl Typer {
    pub fn type_check(
        &mut self,
        var_env: VarEnv,
        expr: UntypedExpr,
    ) -> Result<TypedExpr, TypeError> {
        let (typed_expr, constraints) = self.infer(var_env, expr)?;
        let subst = Self::unify(constraints)?;
        Ok(typed_expr.apply(&subst))
    }

    fn infer(
        &mut self,
        mut var_env: VarEnv,
        expr: UntypedExpr,
    ) -> Result<(TypedExpr, Vec<Constraint>), TypeError> {
        match expr {
            UntypedExpr::Var(id, _) => {
                let scheme = var_env
                    .0
                    .get(&id)
                    .ok_or_else(|| TypeError::UnboundVariable(id.clone()))?
                    .clone();

                let qual_type = self.instantiate(scheme)?;
                Ok((TypedExpr::Var(id, qual_type), vec![]))
            }
            UntypedExpr::Lit(lit, ()) => Ok(self.infer_lit(lit)),
            UntypedExpr::App(fn_expr, arg_expr, ()) => {
                let (typed_fn_expr, mut fn_constraints) = self.infer(var_env.clone(), *fn_expr)?;
                let (typed_arg_expr, mut arg_constraints) = self.infer(var_env, *arg_expr)?;
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
            UntypedExpr::Let(name, (), e1, e2) => {
                let (typed_e1, mut e1_constraints) = self.infer(var_env.clone(), *e1)?;
                let ty_e1 = typed_e1.clone().get_type();
                let scheme = self.generalize(var_env.clone(), ty_e1, e1_constraints.clone())?;
                var_env.0.insert(name.clone(), scheme);

                let (typed_e2, mut e2_constraints) = self.infer(var_env, *e2)?;
                let ty_e2 = typed_e2.clone().get_type();

                let mut constraints = vec![];
                constraints.append(&mut e1_constraints);
                constraints.append(&mut e2_constraints);

                let typed_expr =
                    TypedExpr::Let(name, ty_e2, Box::new(typed_e1), Box::new(typed_e2));

                Ok((typed_expr, constraints))
            }
            // TODO: it's still possible that we have bugs here:
            // - we're always creating param_tyvar with kind Star
            // - the param_qual_type is also created without any predicates
            UntypedExpr::Lambda(param, (), body_expr) => {
                let param_tyvar = Type::Var(self.gen_state.gen_fresh(Kind::Star));
                let param_qual_type = QualType::new(vec![], param_tyvar.clone());
                var_env
                    .0
                    .insert(param.clone(), Self::dont_generalize(param_qual_type));

                let (typed_body_expr, constraints) = self.infer(var_env, *body_expr)?;
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

    fn generalize(
        &mut self,
        var_env: VarEnv,
        qual_type: QualType,
        constraints: Vec<Constraint>,
    ) -> Result<Scheme, TypeError> {
        let subst = Self::unify(constraints)?;
        let principal_type = qual_type.apply(&subst);
        let var_env = var_env.apply(&subst);

        let var_env_ftv = var_env.ftv();

        // diff between ftvs in principal type and all quantified vars
        let vars = principal_type
            .clone()
            .ftv()
            .into_iter()
            .filter(|tyvar| !var_env_ftv.contains(tyvar))
            .collect::<Vec<_>>();

        let preds = self.solve_preds(principal_type.clone().preds())?;

        let principal_type = QualType::new(preds, principal_type.clone().ty());

        Ok(Scheme(vars, principal_type))
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

    fn unify(constraints: Vec<Constraint>) -> Result<Subst, TypeError> {
        let substs = constraints
            .into_iter()
            .map(|constr| Unifier::mgu(constr))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Subst::merge(substs))
    }

    // TODO: handle deferred/retained preds & ambiguity
    /// returns a TypeError in case it cannot find a suitable instance for a given predicate
    pub fn solve_preds(&self, preds: Vec<Pred>) -> Result<Vec<Pred>, TypeError> {
        Ok(preds)
    }
}
