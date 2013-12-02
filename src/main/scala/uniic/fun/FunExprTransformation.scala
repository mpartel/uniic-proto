package uniic.fun

/** Provides convenience when recursing over FunExprs. */
trait FunExprTransformation {
  protected type Env

  protected def recursesOverLetBindings = true

  protected def apply(expr: FunExpr, env: Env): FunExpr = {
    expr match {
      case FunUnit => expr
      case FunBool(_) => expr
      case FunInt(_) => expr
      case FunTupleExpr(members) => FunTupleExpr(members.map(apply(_, env)))
      case _: FunBuiltinFunction => expr
      case _: FunBuiltinValue => expr
      case FunLambda(params, body) => FunLambda(params, apply(body, env))
      case FunVar(_) => expr
      case FunApply(fun, args) => FunApply(apply(fun, env), args.map(apply(_, env)))
      case FunLet(bindings, body) => {
        val newBindings = {
          if (recursesOverLetBindings) recurseOverBindings(bindings, env)
          else bindings
        }
        FunLet(newBindings, apply(body, env))
      }
      case FunIfThenElse(cond, thenBody, elseBody) => {
        FunIfThenElse(apply(cond, env), apply(thenBody, env), apply(elseBody, env))
      }
      case FunMatch(head, clauses) => FunMatch(apply(head, env), recurseOverClauses(clauses, env))
    }
  }

  protected def recurseOverBindings(bindings: Seq[(String, FunExpr)], env: Env): Seq[(String, FunExpr)] = {
    bindings.map { case (x, e) => (x, apply(e, env)) }
  }

  protected def recurseOverClauses(clauses: Seq[FunCase], env: Env): Seq[FunCase] = {
    clauses.map { case FunCase(p, e) => FunCase(p, apply(e, env)) }
  }
}
