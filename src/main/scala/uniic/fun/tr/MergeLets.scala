package uniic.fun.tr
import uniic.fun._
import uniic.fun.analysis.FunTree

/** Merges `let x = ... in let y = ...` to `let x = ... and y = ...`
  * unless x == y. */
object MergeLets extends FunExprTransformation {
  protected type Env = Unit // Unused

  def apply(tree: FunTree): FunTree = new FunTree(apply(tree.rootExpr, Unit))
  def apply(expr: FunExpr): FunExpr = apply(expr, Unit)

  override def apply(expr: FunExpr, env: Env): FunExpr = {
    expr match {
      case FunLet(outerBindings, FunLet(innerBindings, innerBody)) => {
        val (incompatibleInners, compatibleInners) = innerBindings.partition(b => outerBindings.exists(_._1 == b._1))
        val newOuterBindings = outerBindings ++ compatibleInners
        val newInnerBindings = incompatibleInners
        if (newInnerBindings.isEmpty) {
          apply(FunLet(newOuterBindings, innerBody))
        } else {
          FunLet(recurseOverBindings(newOuterBindings, env), apply(FunLet(newInnerBindings, innerBody), env))
        }
      }
      case _ => super.apply(expr, env)
    }
  }
}