use crate::typer::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Id(String);

impl Id {
    pub fn new(s: &str) -> Self {
        Id(s.to_owned())
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

pub type UntypedExpr = Expr<()>;
pub type TypedExpr = Expr<Type>;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<T> {
    Var(Id),
    Lit(Literal),
    App(Box<Expr<T>>, Box<Expr<T>>),
    Let(Id, T, Box<Expr<T>>, Box<Expr<T>>),
    Lambda(Id, T, Box<Expr<T>>),
}

// TODO: implement TryFrom<ParsedExpr> for Expr<()>
