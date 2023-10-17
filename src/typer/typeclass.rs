use crate::types::{Qual, Pred};

#[derive(Debug, PartialEq, Clone)]
pub struct TypeClass {
    definition: Qual<Pred>,
    instances: Vec<TypeClassInstance>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeClassInstance {
    definition: Qual<Pred>,
}
