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
            // as we don't allow partial application of arrows (e.g. "(->) Int" in Haskell),
            // we can safely assume that arrow expressions have kind "*"
            Type::Arrow(_, _) => Ok(Kind::Star),
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

/// Qualified types - types with predicates
/// e.g.: "(Eq a, Eq b) => a -> b -> Bool"
pub type QualType = Qual<Type>;

/// Represents a type qualifier
/// - A function type, e.g.: "(Eq a, Eq b) => a -> b -> Bool"
/// - A type class definition, e.g.: "class Applicative m => Monad m where"
/// - A instance declaration, e.g.: "instance (Ord a, Ord b) => Ord (a, b) where"
///
/// The pieces before the "=>" are the predicates, and the pieces after are generic (e.g. a function, a type class, or an instance declaration).
#[derive(Debug, PartialEq, Clone)]
pub struct Qual<T>(Vec<Pred>, T);

/// Represents a predicate, e.g.: "Eq a"
#[derive(Debug, PartialEq, Clone)]
pub struct Pred(Id, Type);
