use crate::ast::Id;

#[derive(Debug, PartialEq, Clone)]
pub enum Kind {
    Star,
    KFun(Box<Kind>, Box<Kind>),
}

enum KindError {
    KindMismatch,
}

trait HasKind {
    fn kind(&self) -> Result<Kind, KindError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Var(TyVar),
    Con(TyCon),
    App(Box<Type>, Box<Type>),
    // TODO: design arrows as a type application of (->) of kind * -> * -> *
    // (like typing haskell in haskell)
    Arrow(Box<Type>, Box<Type>),
}

impl HasKind for Type {
    fn kind(&self) -> Result<Kind, KindError> {
        match self {
            Type::Var(tyvar) => tyvar.kind(),
            Type::Con(tycon) => tycon.kind(),
            Type::App(t, _) => match t.kind()? {
                Kind::KFun(_, k) => Ok(*k),
                _ => Err(KindError::KindMismatch),
            },
            Type::Arrow(_, _) => {
                // TODO: hopefully this will not shoot me in the foot when I do partial type applications

                // for now it's probably safe to assume that arrows have kind *
                Ok(Kind::Star)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TyVar(Id, Kind);

impl HasKind for TyVar {
    fn kind(&self) -> Result<Kind, KindError> {
        Ok(self.1.clone())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TyCon(Id, Kind);

impl HasKind for TyCon {
    fn kind(&self) -> Result<Kind, KindError> {
        Ok(self.1.clone())
    }
}
