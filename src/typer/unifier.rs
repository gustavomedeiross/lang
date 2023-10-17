use crate::types::{Subst, Type, HasKind, HasFreeTypeVariables, KindError, Substitutes, TyVar};

use super::{Constraint, TypeError};

pub struct Unifier;

impl Unifier {
    pub fn mgu(constr: Constraint) -> Result<Subst, TypeError> {
        match constr {
            Constraint(Type::App(l1, r1), Type::App(l2, r2)) => {
                let s1 = Self::mgu(Constraint(*l1, *l2))?;
                let s2 = Self::mgu(Constraint((*r1).apply(&s1), (*r2).apply(&s1)))?;
                Ok(s2.compose(s1))
            }
            Constraint(Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
                let s1 = Self::mgu(Constraint(*l1, *l2))?;
                let s2 = Self::mgu(Constraint((*r1).apply(&s1), (*r2).apply(&s1)))?;
                Ok(s2.compose(s1))
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
