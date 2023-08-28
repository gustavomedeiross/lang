- environment to generate type variables (var state)
- environment for type classes
- assumptions [(Id, Scheme)] or [(Id, QualType)] or [(TyVar, QualType)]

pipeline

```
type TypeEnv = (VarState, TypeClassEnv, Assumptions)
type Prelude = (TypeClassEnv, Assumptions)
type Subst = [(TyVar, Type)]
```


```
// or we could also return just TypedExpr, considering that we "flattened" the type class env in the TypeExpr itself
def typer : Prelude -> UntypedExpr -> (TypedExpr, TypeClassEnv)
```

```
// this can have TGen values in the middle of the expression (look at the definition of thio::quantify)
defp infer : Prelude -> UntypedExpr -> (Typer::Expr<QualType>, [Constraint])

defp unify : [Constraint] -> Subst

defp something : Subst -> Typer::Expr<QualType> -> TypedExpr
```
