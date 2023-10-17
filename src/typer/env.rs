use std::collections::HashMap;

use crate::{
    ast::Id,
    types::{
        HasFreeTypeVariables, Pred, Qual, Scheme, Subst, Substitutes, TyVar, TypeClass,
        TypeClassInstance,
    },
};

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
        VarEnv(
            self.0
                .clone()
                .into_iter()
                .map(|(id, scheme)| (id, scheme.apply(subst)))
                .collect::<HashMap<_, _>>(),
        )
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

// type classes

pub struct TypeClassEnv {
    type_classes: HashMap<Id, TypeClass>,
}

impl TypeClassEnv {
    pub fn new() -> Self {
        Self {
            type_classes: HashMap::new(),
        }
    }

    // TODO: add checks - should return Result
    pub fn add_typeclass(&mut self, definition: Qual<Pred>) {
        let type_class_id = definition.clone().pred().id();
        let type_class = TypeClass::new(definition);
        let is_new = self
            .type_classes
            .insert(type_class_id, type_class)
            .is_none();

        if !is_new {
            panic!("Error adding type class: class was already defined in the env");
        }
    }

    // TODO: add checks - should return Result
    pub fn add_instance(&mut self, definition: Qual<Pred>) {
        let type_class_id = definition.clone().pred().id();

        let type_class = self
            .type_classes
            .get_mut(&type_class_id)
            .expect("Failed to add instance: class not found");

        let instance = TypeClassInstance::new(definition);

        type_class.add_instance(instance)
    }
}
