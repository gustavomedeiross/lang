mod ast;
mod lexer;
#[allow(dead_code)]
mod parser;
mod simplifier;
mod typer;
mod types;

#[cfg(test)]
mod typer_tests;

fn infer(input: &str) -> Result<ast::TypedExpr, typer::TypeError> {
    let parsed = parser::parse_expr(input).expect("parsing failed");
    let expr = simplifier::simplify(*parsed).expect("simplification failed");
    let mut typer = typer::Typer::new(typer::Prelude(typer::TypeClassEnv, vec![]));
    typer.type_check(expr)
}

fn main() {
    let expr = r#"
       let id = fun x -> x
    "#;

    match infer(expr) {
        Ok(typed_expr) => println!("typed_expr: {:?}", typed_expr),
        Err(err) => println!("err: {:?}", err),
    }
}
