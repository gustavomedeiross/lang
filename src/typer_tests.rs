use crate::{
    ast::{Expr, Id, Literal, TypedExpr},
    parser, simplifier,
    typer::{Assumption, Prelude, TypeClassEnv, TypeError, Typer},
    types::{Kind, Pred, QualType, Scheme, TyCon, TyVar, Type},
};

fn infer(input: &str) -> Result<TypedExpr, TypeError> {
    let parsed = parser::parse(input).expect("parsing failed");
    let expr = simplifier::simplify(*parsed).expect("simplification failed");
    let mut typer = Typer::new(default_prelude());
    typer.type_check(expr)
}

// TODO: refactor this
fn default_prelude() -> Prelude {
    let assumptions = vec![
        // show : forall a . Show a => a -> String
        Assumption(
            Id::new("show"),
            Scheme(
                vec![TyVar(Id::new("a"), Kind::Star)],
                QualType::new(
                    vec![Pred::new(
                        Id::new("Show"),
                        Type::Var(TyVar(Id::new("a"), Kind::Star)),
                    )],
                    Type::Arrow(
                        Box::new(Type::Var(TyVar(Id::new("a"), Kind::Star))),
                        Box::new(Type::Con(TyCon(Id::new("String"), Kind::Star))),
                    ),
                ),
            ),
        ),
        // increment : Int -> Int
        Assumption(
            Id::new("increment"),
            Scheme(
                vec![],
                QualType::new(
                    vec![],
                    Type::Arrow(
                        Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
                        Box::new(Type::Con(TyCon(Id::new("Int"), Kind::Star))),
                    ),
                ),
            ),
        ),
        // true : bool
        Assumption(
            Id::new("true"),
            Scheme(
                vec![],
                QualType::new(vec![], Type::Con(TyCon(Id::new("Bool"), Kind::Star))),
            ),
        ),
        // false : bool
        Assumption(
            Id::new("false"),
            Scheme(
                vec![],
                QualType::new(vec![], Type::Con(TyCon(Id::new("Bool"), Kind::Star))),
            ),
        ),
    ];

    Prelude(TypeClassEnv, assumptions)
}

#[test]
fn test_literal_num() -> Result<(), TypeError> {
    let typed_expr = infer("1")?.get_type().to_string();
    let expected = "Num t0 => t0".to_owned();
    assert_eq!(typed_expr, expected);
    Ok(())
}

// TODO: test
//
// "x -> y -> y + y" `yields` Num t1 => t0 -> t1 -> t1
//
// "let f x = show x in f" `yields` Show a => a -> String
//
// "show(true)" `yields` missing typeclass instance: Show(bool)
//
// "let f x = show x in f true" `yields` "missing typeclass instance: Show(bool)
//
// let f = fun x -> let y = show(x) in x in f `yields` Show a => a -> a
