use crate::types::{self, Substitutes};
use std::fmt;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Id(String);

impl Id {
    pub fn new(s: &str) -> Self {
        Id(s.to_owned())
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Str(String),
}

// at some point we want to support patterns as arguments
// (but not on lambdas probably)
pub type Args = Vec<Id>;

#[derive(Debug, PartialEq, Clone)]
pub enum ParsedExpr {
    Var(Id),
    Lit(Literal),
    // TODO: Vec<Box<T>> is redundant, so probably just use Vec<T>
    App(Box<ParsedExpr>, Vec<Box<ParsedExpr>>),
    // TODO: add support for multiple bindings in a single let
    Let(Id, Box<ParsedExpr>, Box<ParsedExpr>),
    Lambda(Args, Box<ParsedExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<T> {
    Var(Id, T),
    Lit(Literal, T),
    App(Box<Expr<T>>, Box<Expr<T>>, T),
    Let(Id, T, Box<Expr<T>>, Box<Expr<T>>),
    Lambda(Id, T, Box<Expr<T>>),
}

impl<T: Substitutes> types::Substitutes for Expr<T> {
    fn apply(self, subst: &types::Subst) -> Self {
        match self {
            Expr::Var(id, ty) => Expr::Var(id, ty.apply(subst)),
            Expr::Lit(lit, ty) => Expr::Lit(lit, ty.apply(subst)),
            Expr::App(e1, e2, ty) => Expr::App(
                Box::new(e1.apply(subst)),
                Box::new(e2.apply(subst)),
                ty.apply(subst),
            ),
            Expr::Let(id, ty, e1, e2) => Expr::Let(
                id,
                ty.apply(subst),
                Box::new(e1.apply(subst)),
                Box::new(e2.apply(subst)),
            ),
            Expr::Lambda(id, ty, e) => Expr::Lambda(id, ty.apply(subst), Box::new(e.apply(subst))),
        }
    }
}

pub type UntypedExpr = Expr<()>;
pub type TypedExpr = Expr<types::QualType>;

impl TypedExpr {
    pub fn get_type(self) -> types::QualType {
        match self {
            Expr::Var(_, ty) => ty,
            Expr::Lit(_, ty) => ty,
            Expr::App(_, _, ty) => ty,
            Expr::Let(_, ty, _, _) => ty,
            Expr::Lambda(_, ty, _) => ty,
        }
    }

    pub fn stringify_types(self) -> Expr<String> {
        match self {
            Expr::Var(id, ty) => Expr::Var(id, ty.to_string()),
            Expr::Lit(lit, ty) => Expr::Lit(lit, ty.to_string()),
            Expr::App(e1, e2, ty) => Expr::App(
                Box::new(e1.stringify_types()),
                Box::new(e2.stringify_types()),
                ty.to_string(),
            ),
            Expr::Let(id, ty, e1, e2) => Expr::Let(
                id,
                ty.to_string(),
                Box::new(e1.stringify_types()),
                Box::new(e2.stringify_types()),
            ),
            Expr::Lambda(id, ty, e) => {
                Expr::Lambda(id, ty.to_string(), Box::new(e.stringify_types()))
            }
        }
    }
}
