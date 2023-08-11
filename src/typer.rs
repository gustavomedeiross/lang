use crate::ast::Expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeError {}

pub fn typer(_input: Expr<()>) -> Result<Expr<Type>, TypeError> {
    todo!()
}
