use crate::{
    ast::{Id, TypedExpr},
    parser, simplifier,
    typer::{Assumption, TypeClassEnv, TypeError, Typer},
};

fn infer(input: &str) -> Result<TypedExpr, TypeError> {
    let parsed = parser::parse_expr(input).expect("parsing failed");
    let expr = simplifier::simplify(*parsed).expect("simplification failed");
    let (type_class_env, assumptions) = default_env();
    let mut typer = Typer::new(type_class_env);
    typer.type_check(assumptions.into(), expr)
}

fn assumption(id: &str, scheme: &str) -> Assumption {
    Assumption(
        Id::new(id),
        parser::parse_type_scheme(scheme).expect("parsing of type scheme failed"),
    )
}

fn add_typeclass(type_class_env: &mut TypeClassEnv, qual_pred: &str) {
    let qual_pred = parser::parse_qual_pred_expr(qual_pred)
        .expect("add_typeclass failure: parsing of qual pred failed");
    type_class_env.add_typeclass(qual_pred);
}

fn add_instance(type_class_env: &mut TypeClassEnv, qual_pred: &str) {
    let qual_pred = parser::parse_qual_pred_expr(qual_pred)
        .expect("add_instance failure: parsing of qual pred failed");
    type_class_env.add_instance(qual_pred);
}

fn default_env() -> (TypeClassEnv, Vec<Assumption>) {
    let mut type_class_env = TypeClassEnv::new();
    add_typeclass(&mut type_class_env, "Show a");

    let assumptions = vec![
        assumption("true", "Bool"),
        assumption("false", "Bool"),
        assumption("show", "a . (Show a) => a -> String"),
        assumption("increment", "Int -> Int"),
        assumption("one", "Int"),
        // NoShow doesn't implement "Show"
        assumption("noShow", "NoShow"),
    ];

    add_instance(&mut type_class_env, "Show Bool");
    add_instance(&mut type_class_env, "Show String");
    add_instance(&mut type_class_env, "Show Int");

    (type_class_env, assumptions)
}

mod principal_type {
    use super::infer;
    use crate::parser;

    fn types_to(input: &str, expected: &str) {
        let typed_expr = infer(&input).expect("failed to infer type").get_type();
        let expected = parser::parse_qual_type_expr(expected).expect("parsing of qual_type failed");
        assert_eq!(typed_expr, expected);
    }

    #[test]
    fn test_literals() {
        types_to("1", "(Num t0) => t0");
        types_to(r#""hello""#, "List Char");
    }

    #[test]
    fn test_let() {
        types_to("let x = true in x", "Bool");
        types_to("let x = 2 in increment x", "(Num Int) => Int");
        // TODO: add more tests
    }

    #[test]
    fn test_let_polymorphism() {
        types_to("let id = fun x -> x in id", "t1 -> t1");
        types_to(
            r#"
            let id = fun x -> x in
            let int = id one in
            id
        "#,
            "t4 -> t4",
        );
        types_to(
            r#"
            let id = fun x -> x in
            let int = id one in
            let bool = id true in
            int
        "#,
            "Int",
        );
        types_to(
            r#"
            let id = fun x -> x in
            let int = id one in
            let bool = id true in
            bool
        "#,
            "Bool",
        );
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
}

mod typeclasses {
    use super::infer;
    use crate::parser;

    fn types_to(input: &str, expected: &str) {
        let typed_expr = infer(&input).expect("failed to infer type").get_type();
        let expected = parser::parse_qual_type_expr(expected).expect("parsing of qual_type failed");
        assert_eq!(typed_expr, expected);
    }

    fn returns_error(input: &str, expected: &str) {
        let type_error = infer(&input).expect_err("expected type error, got");
        assert_eq!(type_error.to_string(), expected.to_owned());
    }

    #[test]
    fn infers_type_qualifiers() {
        types_to("show", "(Show t0) => t0 -> String");
        types_to("let f = fun x -> show x in f", "(Show t3) => t3 -> String");
    }

    #[test]
    fn resolves_predicates() {
        returns_error(
            "noShow",
            "Type error: missing typeclass instance: Show(NoShow)",
        );
    }
}

mod typed_expr {
    use super::infer;
    use crate::{
        ast::{Expr, Id, Literal, TypedExpr},
        parser, types,
    };

    fn infers(input: &str, expected: TypedExpr) {
        let typed_expr = infer(&input).expect("failed to infer type");
        assert_eq!(typed_expr, expected);
    }

    fn qual_type(ty: &str) -> types::QualType {
        parser::parse_qual_type_expr(ty).expect("parsing failed")
    }

    #[test]
    fn test_literal_num() {
        let expr = "1";
        let expected = Expr::Lit(Literal::Int(1), qual_type("(Num t0) => t0"));
        infers(expr, expected)
    }

    #[test]
    fn test_literal_string() {
        let expr = r#""hello""#;
        let expected = Expr::Lit(
            Literal::Str(r#""hello""#.to_owned()),
            qual_type("List Char"),
        );
        infers(expr, expected)
    }

    #[test]
    fn test_lambda_identity() {
        let expr = "fun x -> x";
        let expected = Expr::Lambda(
            Id::new("x"),
            qual_type("t0 -> t0"),
            Box::new(Expr::Var(Id::new("x"), qual_type("t0"))),
        );
        infers(expr, expected)
    }

    #[test]
    fn test_lambda_with_application() {
        let expr = "fun x -> increment x";
        let expected = Expr::Lambda(
            Id::new("x"),
            qual_type("Int -> Int"),
            Box::new(Expr::App(
                Box::new(Expr::Var(Id::new("increment"), qual_type("Int -> Int"))),
                Box::new(Expr::Var(Id::new("x"), qual_type("Int"))),
                qual_type("Int"),
            )),
        );
        infers(expr, expected)
    }

    #[test]
    fn test_basic_let_expr() {
        let expr = "let x = true in x";
        let expected = Expr::Let(
            Id::new("x"),
            qual_type("Bool"),
            Box::new(Expr::Var(Id::new("true"), qual_type("Bool"))),
            Box::new(Expr::Var(Id::new("x"), qual_type("Bool"))),
        );
        infers(expr, expected)
    }

    // type classes

    #[test]
    fn test_lambda_with_application_of_qual_type() {
        let expr = "fun x -> show x";
        let expected = Expr::Lambda(
            Id::new("x"),
            qual_type("(Show t0) => t0 -> String"),
            Box::new(Expr::App(
                Box::new(Expr::Var(
                    Id::new("show"),
                    qual_type("(Show t0) => t0 -> String"),
                )),
                // TODO: should this have "Show a" or not?
                // it's not strictly required, as this structure is only going to be used for coge generation
                Box::new(Expr::Var(Id::new("x"), qual_type("t0"))),
                qual_type("(Show t0) => String"),
            )),
        );
        infers(expr, expected)
    }
}
