use crate::ast::Id;
use std::fmt;

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
    // TODO: we should have another variation of "Type"
    // without "Gen"
    Gen(TGen),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var(tyvar) => write!(f, "{}", tyvar.0),
            Type::Con(tycon) => write!(f, "{}", tycon.0),
            Type::App(t, u) => write!(f, "{} {}", t, u),
            Type::Arrow(t, u) => write!(f, "{} -> {}", t, u),
            Type::Gen(tgen) => write!(f, "TGen: {}", tgen.0),
        }
    }
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
            Type::Gen(_) => panic!("Type::Gen should not be used in typechecking"),
        }
    }
}

// TODO: maybe should be something like `Scheme(Vec<(TGen, Kind)>, QualType)`
#[derive(Debug, PartialEq, Clone)]
pub struct Scheme(pub Vec<TyVar>, pub QualType);

#[derive(Debug, PartialEq, Clone)]
pub struct TGen(usize);

impl TGen {
    pub fn initial() -> Self {
        TGen(0)
    }

    pub fn next(self) -> Self {
        TGen(self.0 + 1)
    }

    pub fn to_tyvar(&self, kind: Kind) -> TyVar {
        TyVar::from_int(self.0, kind)
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct TyVar(pub Id, pub Kind);

impl TyVar {
    pub fn from_int(int: usize, kind: Kind) -> Self {
        let id = Id::new(&format!("t{}", int));
        TyVar(id, kind)
    }
}

impl HasKind for TyVar {
    fn kind(&self) -> Result<Kind, KindError> {
        Ok(self.1.clone())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TyCon(pub Id, pub Kind);

impl HasKind for TyCon {
    fn kind(&self) -> Result<Kind, KindError> {
        Ok(self.1.clone())
    }
}

/// Qualified types - types with predicates
/// e.g.: "(Eq a, Eq b) => a -> b -> Bool"
pub type QualType = Qual<Type>;

impl fmt::Display for QualType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let preds = self
            .0
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        if preds.is_empty() {
            return write!(f, "{}", self.1);
        }
        write!(f, "{} => {}", preds, self.1)
    }
}

/// Represents a type qualifier
/// - A function type, e.g.: "(Eq a, Eq b) => a -> b -> Bool"
/// - A type class definition, e.g.: "class Applicative m => Monad m where"
/// - A instance declaration, e.g.: "instance (Ord a, Ord b) => Ord (a, b) where"
///
/// The pieces before the "=>" are the predicates, and the pieces after are generic (e.g. a function, a type class, or an instance declaration).
#[derive(Debug, PartialEq, Clone)]
pub struct Qual<T>(Vec<Pred>, T);

impl<T> Qual<T> {
    pub fn new(preds: Vec<Pred>, t: T) -> Self {
        Qual(preds, t)
    }
}

/// Represents a predicate, e.g.: "Eq a"
#[derive(Debug, PartialEq, Clone)]
pub struct Pred(Id, Type);

impl Pred {
    pub fn new(id: Id, ty: Type) -> Self {
        Pred(id, ty)
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.0, self.1)
    }
}

pub mod prelude {
    use super::*;

    pub fn t_char() -> Type {
        Type::Con(TyCon(Id::new("Char"), Kind::Star))
    }

    pub fn t_list() -> Type {
        Type::Con(TyCon(
            Id::new("List"),
            Kind::KFun(Box::new(Kind::Star), Box::new(Kind::Star)),
        ))
    }

    fn list(t: Type) -> Type {
        Type::App(Box::new(t_list()), Box::new(t))
    }

    pub fn t_string() -> Type {
        list(t_char())
    }
}

#[cfg(test)]
mod test {
    use super::{prelude::*, *};

    #[test]
    fn test_display_prelude() {
        assert_eq!(format!("{}", t_char()), "Char");
        assert_eq!(format!("{}", t_list()), "List");
        assert_eq!(format!("{}", t_string()), "List Char");
    }

    #[test]
    fn test_display_qual_type() {
        let ty_var = Type::Var(TyVar(Id::new("a"), Kind::Star));
        let pred = Pred::new(Id::new("Num"), ty_var.clone());
        let qual_type = Qual::new(vec![pred], ty_var);

        assert_eq!(format!("{}", qual_type), "Num a => a");
    }

    #[test]
    fn test_display_qual_type_without_predicates() {
        let qual_type = Qual::new(vec![], t_string());

        assert_eq!(format!("{}", qual_type), "List Char");
    }
}
