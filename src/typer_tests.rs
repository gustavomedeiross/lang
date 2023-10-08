use crate::{
    ast::{Id, TypedExpr},
    parser, simplifier,
    typer::{Assumption, Prelude, TypeClassEnv, TypeError, Typer},
};

fn infer(input: &str) -> Result<TypedExpr, TypeError> {
    let parsed = parser::parse_expr(input).expect("parsing failed");
    let expr = simplifier::simplify(*parsed).expect("simplification failed");
    let mut typer = Typer::new(default_prelude());
    typer.type_check(expr)
}

fn assumption(id: &str, scheme: &str) -> Assumption {
    Assumption(
        Id::new(id),
        parser::parse_type_scheme(scheme).expect("parsing failed"),
    )
}

fn default_prelude() -> Prelude {
    let assumptions = vec![
        assumption("show", "a . (Show a) => a -> String"),
        assumption("increment", "Int -> Int"),
        assumption("true", "Bool"),
        assumption("false", "Bool"),
    ];

    Prelude(TypeClassEnv, assumptions)
}

mod principal_type {
    use super::infer;
    use crate::typer::TypeError;

    #[test]
    fn test_literal_num() -> Result<(), TypeError> {
        let typed_expr = infer("1")?.get_type().to_string();
        let expected = "Num t0 => t0".to_owned();
        assert_eq!(typed_expr, expected);
        Ok(())
    }
}

mod typed_expr {
    use super::infer;
    use crate::{
        ast::{Expr, Id, Literal},
        typer::TypeError,
    };

    #[test]
    fn test_literal_num() -> Result<(), TypeError> {
        let typed_expr = infer("1")?.stringify_types();
        let qual_type = "Num t0 => t0".to_owned();
        assert_eq!(typed_expr, Expr::Lit(Literal::Int(1), qual_type));
        Ok(())
    }

    #[test]
    fn test_literal_string() -> Result<(), TypeError> {
        let typed_expr = infer(r#""hello""#)?.stringify_types();
        let qual_type = "List Char".to_owned();
        assert_eq!(
            typed_expr,
            Expr::Lit(Literal::Str(r#""hello""#.to_owned()), qual_type)
        );
        Ok(())
    }

    #[test]
    fn test_lambda_identity() -> Result<(), TypeError> {
        let typed_expr = infer("fun x -> x")?.stringify_types();
        assert_eq!(
            typed_expr,
            Expr::Lambda(
                Id::new("x"),
                "t0 -> t0".to_string(),
                Box::new(Expr::Var(Id::new("x"), "t0".to_string()))
            )
        );
        Ok(())
    }

    #[test]
    fn test_lambda_with_application() -> Result<(), TypeError> {
        let typed_expr = infer("fun x -> increment x")?.stringify_types();
        assert_eq!(
            typed_expr,
            Expr::Lambda(
                Id::new("x"),
                "Int -> Int".into(),
                Box::new(Expr::App(
                    Box::new(Expr::Var(Id::new("increment"), "Int -> Int".into())),
                    Box::new(Expr::Var(Id::new("x"), "Int".into())),
                    "Int".into(),
                ))
            )
        );

        Ok(())
    }

    #[test]
    #[ignore]
    fn test_basic_let_expr() -> Result<(), TypeError> {
        let typed_expr = infer("let x = true in x")?.stringify_types();
        assert_eq!(
            typed_expr,
            Expr::Let(
                Id::new("x"),
                "Bool".into(),
                Box::new(Expr::Var(Id::new("true"), "Bool".into())),
                Box::new(Expr::Var(Id::new("x"), "Bool".into())),
            )
        );

        Ok(())
    }

    // type classes

    #[test]
    fn test_lambda_with_application_of_qual_type() -> Result<(), TypeError> {
        let typed_expr = infer("fun x -> show x")?.stringify_types();
        assert_eq!(
            typed_expr,
            Expr::Lambda(
                Id::new("x"),
                "Show t0 => t0 -> String".into(),
                Box::new(Expr::App(
                    Box::new(Expr::Var(Id::new("show"), "Show t0 => t0 -> String".into())),
                    // TODO: should this have "Show a" or not?
                    // it's not strictly required, as this structure is only going to be used for coge generation
                    Box::new(Expr::Var(Id::new("x"), "t0".into())),
                    "Show t0 => String".into(),
                ))
            )
        );

        Ok(())
    }

    // #[test]
    // fn test_application_of_qual_type_without_instance() -> Result<(), TypeError> {
    //     let typed_expr = infer("show true")?.stringify_types();
    //     assert_eq!(
    //         typed_expr,
    //         Expr::Lambda(
    //             Id::new("x"),
    //             "Show t0 => t0 -> String".into(),
    //             Box::new(Expr::App(
    //                 Box::new(Expr::Var(Id::new("show"), "Show t0 => t0 -> String".into())),
    //                 // TODO: should this have "Show a" or not?
    //                 // it's not strictly required, as this structure is only going to be used for coge generation
    //                 // so whatever works best, if we prefer to remove the "Show" predicate (from the Expr::Var)
    //                 // we can do that.
    //                 Box::new(Expr::Var(Id::new("x"), "Show t0 => t0".into())),
    //                 "String".into(),
    //             ))
    //         )
    //     );

    //     Ok(())
    // }

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
}
