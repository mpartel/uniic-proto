package uniic.fun.tr
import uniic.fun._
import uniic.fun.analysis.FunTree

/** Creates top-level let-bindings for constants used more than once. */
object ConstantSharing {
  private type Counts = Map[FunLiteral, Int]
  private object Counts {
    val empty = Map.empty[FunLiteral, Int]
  }

  def apply(tree: FunTree): FunTree = {
    val counts = countConstants(tree)(tree.rootNode).toSeq
    val bindings = for ((v, c) <- counts if c > 1) yield v.literalIdentifier -> v
    if (!bindings.isEmpty) {
      val bindingMap = bindings.map(_.swap).toMap
      val replaced = ReplaceInstances(tree.rootExpr, bindingMap)
      new FunTree(FunLet(bindings.sortBy(_._1), replaced))
    } else {
      tree
    }
  }

  private object ReplaceInstances extends FunExprTransformation {
    protected type Env = Map[FunLiteral, String]
    protected val Env = Map
    override def apply(expr: FunExpr, env: Env): FunExpr = {
      expr match {
        case c: FunLiteral if env.contains(c) => FunVar(env(c))
        case _ => super.apply(expr, env)
      }
    }
  }

  private def countConstants(tree: FunTree)(n: tree.Node): Counts = {
    n.expr match {
      case c: FunLiteral => Map(c -> 1)
      case _ => {
        val subcounts = n.children.map(countConstants(tree))
        subcounts.foldLeft(Counts.empty)(sum(_, _))
      }
    }
  }

  private def sum(left: Counts, right: Counts): Counts = {
    val keys = left.keySet union right.keySet
    keys.map(k => k -> (left.getOrElse(k, 0) + right.getOrElse(k, 0))).toMap
  }
}
