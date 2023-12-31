use crate::ast::Id;
use std::{collections::HashSet, fmt, thread::panicking};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Kind {
    Star,
    KFun(Box<Kind>, Box<Kind>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum KindError {
    KindMismatch,
}

impl std::fmt::Display for KindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KindError::KindMismatch => write!(f, "kind mismatch"),
        }
    }
}

pub trait HasKind {
    fn kind(&self) -> Result<Kind, KindError>;
}

// TODO: maybe remove `pub`
#[derive(Debug, PartialEq, Clone)]
pub struct Subst(pub Vec<(TyVar, Type)>);

pub struct SubstMergeError;

impl Subst {
    pub fn null() -> Self {
        Subst(vec![])
    }

    pub fn bind(tyvar: TyVar, ty: Type) -> Self {
        Subst(vec![(tyvar, ty)])
    }

    pub fn compose(self, other: Subst) -> Self {
        let other = other.apply(&self);

        let mut subst = self.0;
        subst.extend(other.0);
        Subst(subst)
    }

    pub fn merge(self, other: Subst) -> Result<Self, SubstMergeError> {
        let s1_hash = self
            .0
            .clone()
            .into_iter()
            .map(|s| s.0)
            .collect::<HashSet<_>>();
        let s2_hash = other
            .0
            .clone()
            .into_iter()
            .map(|s| s.0)
            .collect::<HashSet<_>>();

        let intersection = s1_hash.intersection(&s2_hash).cloned().collect::<Vec<_>>();

        let agree = intersection
            .into_iter()
            .all(|tyvar| Type::Var(tyvar.clone()).apply(&self) == Type::Var(tyvar).apply(&other));

        if agree {
            let mut subst = self.0;
            subst.extend(other.0);
            Ok(Subst(subst))
        } else {
            Err(SubstMergeError)
        }
    }

    // TODO: should do .compose() on each element ("@@" from THIH)
    pub fn join(substs: Vec<Subst>) -> Self {
        let subst = substs
            .into_iter()
            .map(|s| s.0)
            .flatten()
            .collect::<Vec<_>>();
        Subst(subst)
    }
}

pub trait Substitutes {
    // TODO: should be apply(&mut self, subst: &Subst) -> Unit
    fn apply(self, subst: &Subst) -> Self;
}

impl Substitutes for Subst {
    fn apply(self, subst: &Subst) -> Self {
        let new_subst = self
            .0
            .into_iter()
            .map(|(tyvar, ty)| (tyvar, ty.apply(subst)))
            .collect::<Vec<_>>();

        Subst(new_subst)
    }
}

impl<T: Substitutes> Substitutes for Vec<T> {
    fn apply(self, subst: &Subst) -> Self {
        self.into_iter().map(|t| t.apply(subst)).collect()
    }
}

pub trait HasFreeTypeVariables {
    fn ftv(self) -> Vec<TyVar>;
}

impl<T: HasFreeTypeVariables> HasFreeTypeVariables for Vec<T> {
    fn ftv(self) -> Vec<TyVar> {
        // TODO: transform into set to remove duplicates
        self.into_iter().flat_map(|t| t.ftv()).collect()
    }
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
    #[allow(dead_code)]
    Gen(TGen),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var(tyvar) => write!(f, "{}", tyvar),
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

impl Substitutes for Type {
    fn apply(self, subst: &Subst) -> Self {
        match self {
            Type::Var(ty_var) => subst
                .0
                .iter()
                .find(|(tyvar, _)| tyvar == &ty_var)
                .map(|(_, ty)| ty.clone())
                .unwrap_or_else(|| Type::Var(ty_var)),
            Type::App(l, r) => Type::App(Box::new(l.apply(subst)), Box::new(r.apply(subst))),
            Type::Arrow(l, r) => Type::Arrow(Box::new(l.apply(subst)), Box::new(r.apply(subst))),
            _ => self,
        }
    }
}

impl HasFreeTypeVariables for Type {
    // this is definitely not the most efficient way to do this
    // maybe a visitor with linked lists would be better
    fn ftv(self) -> Vec<TyVar> {
        match self {
            Type::Var(ty_var) => vec![ty_var],
            Type::App(l, r) | Type::Arrow(l, r) => {
                let mut ftv_l = l.ftv();
                let mut ftv_r = r.ftv();
                ftv_l.append(&mut ftv_r);
                ftv_l
            }
            Type::Con(_) | Type::Gen(_) => vec![],
        }
    }
}

// TODO: should probably hold a kind as well
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

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct TyVar(pub Id, pub Kind);

impl TyVar {
    pub fn from_int(int: usize, kind: Kind) -> Self {
        let id = Id::new(&format!("t{}", int));
        TyVar(id, kind)
    }
}

impl std::fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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

impl QualType {
    pub fn ty(self) -> Type {
        self.1
    }
}

/// Qualified predicates - predicate that contains other predicates
/// e.g.: "(Eq a) => Ord a
pub type QualPred = Qual<Pred>;

impl QualPred {
    pub fn pred(self) -> Pred {
        self.1
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

    pub fn preds(self) -> Vec<Pred> {
        self.0
    }
}

impl<T: Substitutes> Substitutes for Qual<T> {
    fn apply(self, subst: &Subst) -> Self {
        let preds = self.0.apply(subst);
        let ty = self.1.apply(subst);
        Qual(preds, ty)
    }
}

impl<T: HasFreeTypeVariables> HasFreeTypeVariables for Qual<T> {
    fn ftv(self) -> Vec<TyVar> {
        let mut ftv_preds = self.0.ftv();
        let mut ftv_ty = self.1.ftv();
        ftv_preds.append(&mut ftv_ty);
        ftv_preds
    }
}

/// Represents a predicate, e.g.: "Eq a"
#[derive(Debug, PartialEq, Clone)]
pub struct Pred(Id, Type);

impl Pred {
    pub fn new(id: Id, ty: Type) -> Self {
        Pred(id, ty)
    }

    pub fn id(self) -> Id {
        self.0
    }

    pub fn ty(self) -> Type {
        self.1
    }

    pub fn in_head_normal_form(&self) -> bool {
        Self::ty_in_hnf(&self.1)
    }

    fn ty_in_hnf(ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::Con(_) => false,
            Type::App(t, _) => Self::ty_in_hnf(&(*t)),
            Type::Arrow(t, _) => Self::ty_in_hnf(&(*t)),
            Type::Gen(_) => panic!("Not implemented for TGen"),
        }
    }
}

impl Substitutes for Pred {
    fn apply(self, subst: &Subst) -> Self {
        let ty = self.1.apply(subst);
        Pred(self.0, ty)
    }
}

impl HasFreeTypeVariables for Pred {
    fn ftv(self) -> Vec<TyVar> {
        self.1.ftv()
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.0, self.1)
    }
}

// TODO: maybe should be something like `Scheme(Vec<(TGen, Kind)>, QualType)`
#[derive(Debug, PartialEq, Clone)]
pub struct Scheme(pub Vec<TyVar>, pub QualType);

impl Scheme {
    pub fn new(tyvars: Vec<TyVar>, qual_type: QualType) -> Self {
        Scheme(tyvars, qual_type)
    }
}

impl Substitutes for Scheme {
    fn apply(self, subst: &Subst) -> Self {
        Scheme(self.0, self.1.apply(subst))
    }
}

impl HasFreeTypeVariables for Scheme {
    fn ftv(self) -> Vec<TyVar> {
        let quantified_vars = self.0;
        let vars_in_expr = self.1.ftv();
        // diff between vars in expr and quantified vars
        // e.g: (forall a b . a -> b -> c) => [c]
        vars_in_expr
            .into_iter()
            .filter(|tyvar| !quantified_vars.contains(tyvar))
            .collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeClass {
    definition: QualPred,
    instances: Vec<TypeClassInstance>,
}

impl TypeClass {
    pub fn new(definition: QualPred) -> Self {
        Self {
            definition,
            instances: vec![],
        }
    }

    pub fn add_instance(&mut self, instance: TypeClassInstance) {
        self.instances.push(instance);
    }

    pub fn instances(self) -> Vec<TypeClassInstance> {
        self.instances
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeClassInstance(pub QualPred);

impl TypeClassInstance {
    pub fn new(definition: QualPred) -> Self {
        Self(definition)
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
