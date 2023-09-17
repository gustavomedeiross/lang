use crate::types::QualType;
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
    App(Box<Expr<T>>, Box<Expr<T>>),
    Let(Id, T, Box<Expr<T>>, Box<Expr<T>>),
    Lambda(Id, T, Box<Expr<T>>),
}

pub type UntypedExpr = Expr<()>;
pub type TypedExpr = Expr<QualType>;

impl TypedExpr {
    pub fn get_type(self) -> QualType {
        match self {
            Expr::Var(_, ty) => ty,
            Expr::Lit(_, ty) => ty,
            Expr::App(_, _) => todo!(),
            Expr::Let(_, _, _, _) => todo!(),
            Expr::Lambda(_, ty, _) => ty,
        }
    }

    pub fn stringify_types(self) -> Expr<String> {
        match self {
            Expr::Var(id, ty) => Expr::Var(id, ty.to_string()),
            Expr::Lit(lit, ty) => Expr::Lit(lit, ty.to_string()),
            Expr::App(e1, e2) => Expr::App(
                Box::new(e1.stringify_types()),
                Box::new(e2.stringify_types()),
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
