use crate::ast::{ParsedExpr, Expr};

pub enum SimplifierError {}

pub fn simplify(_parsed_expr: ParsedExpr) -> Result<Expr<()>, SimplifierError> {
    todo!()
}
