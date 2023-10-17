use crate::types::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Constraint(pub Type, pub Type);
