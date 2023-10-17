use std::collections::HashMap;

use crate::{ast::Id, types::{Scheme, Substitutes, Subst, HasFreeTypeVariables, TyVar, TypeClass}};

pub struct Assumption(pub Id, pub Scheme);

#[derive(Debug, PartialEq, Clone)]
pub struct VarEnv(pub HashMap<Id, Scheme>);

impl From<Vec<Assumption>> for VarEnv {
    fn from(assumptions: Vec<Assumption>) -> Self {
        let var_env = assumptions
            .into_iter()
            .map(|Assumption(id, scheme)| (id, scheme))
            .collect::<HashMap<Id, Scheme>>();

        Self(var_env)
    }
}

impl Substitutes for VarEnv {
    // TODO: fix bad performance when we have the new Subst sig
    fn apply(self, subst: &Subst) -> Self {
        VarEnv(self.0
            .clone()
            .into_iter()
            .map(|(id, scheme)| (id, scheme.apply(subst)))
            .collect::<HashMap<_, _>>())
    }
}

impl HasFreeTypeVariables for VarEnv {
    fn ftv(self) -> Vec<TyVar> {
        self.0
            .into_values()
            .flat_map(|scheme| scheme.ftv())
            .collect()
    }
}

pub struct TypeClassEnv {
    type_classes: HashMap<Id, TypeClass>,
}

impl TypeClassEnv {
    pub fn new() -> Self {
        Self {
            type_classes: HashMap::new()
        }
    }
}

