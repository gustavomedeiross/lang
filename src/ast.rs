// TODO: add optional type annotations

#[derive(Debug, PartialEq, Clone)]
pub struct Id(String);

#[derive(Debug, PartialEq, Clone)]
pub struct Literal(String);

// at some point we want to support patterns as arguments
// (but not on lambdas probably)
pub type Args = Vec<Id>;

#[derive(Debug, PartialEq, Clone)]
pub enum ParsedExpr {
    Var(Id),
    Lit(Literal),
    // TODO: Vec<Box<T>> is redundant, so probably just use Vec<T>
    App(Box<ParsedExpr>, Vec<Box<ParsedExpr>>),
    Let(Vec<Definition>, Box<ParsedExpr>),
    Lambda(Args, Box<ParsedExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    // TODO: maybe "Signature" would be a better name
    TypeAnnotation(Id, Box<ParsedExpr>),
    ValueDefinition(Id, Args, ParsedExpr),
}

