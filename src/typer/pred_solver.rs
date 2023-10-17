use crate::types::{Pred, TyVar};

use super::{TypeClassEnv, TypeError};

#[derive(Debug, PartialEq, Clone)]
struct Solution {
    pub deferred: Vec<Pred>,
    pub retained: Vec<Pred>,
}

struct PredSolver;

impl PredSolver {
    /// returns a TypeError in case it cannot find a suitable instance for a given predicate
    pub fn solve_preds(
        type_class_env: TypeClassEnv,
        // variables appearing free in the assumptions
        fixed_vars: Vec<TyVar>,
        // variables that we would like to quantify
        variables_to_quantify: Vec<TyVar>,
        preds: Vec<Pred>,
    ) -> Result<Solution, TypeError> {
        todo!()
    }
}
