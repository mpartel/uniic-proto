package uniic.fun.analysis
import uniic.fun._

class FunTree(val rootExpr: FunExpr)
  extends FunTreeBase
  with FunTreeGraphviz
  with FunTreeTypeLevelVars
{

  class Node(val expr: FunExpr, val parent: Option[Node])
    extends super[FunTreeBase].NodeBase
    with super[FunTreeTypeLevelVars].TypeLevelVarsNode
  {
    lazy val children: Seq[Node] = {
      def child(e: FunExpr) = new Node(e, Some(this))
      expr match {
        case FunUnit => Seq.empty
        case FunBool(_) => Seq.empty
        case FunInt(_) => Seq.empty
        case FunVar(v) => Seq.empty
        case FunTupleExpr(members) => members.map(child(_))
        case FunLambda(_, body) => Seq(child(body))
        case _: FunBuiltinFunction => Seq.empty
        case _: FunBuiltinValue => Seq.empty
        case FunApply(fun, args) => child(fun) +: args.map(child(_))
        case FunLet(bindings, body) => bindings.map(b => child(b._2)) :+ child(body)
        case FunIfThenElse(cond, thenBody, elseBody) => Seq(cond, thenBody, elseBody).map(child(_))
        case FunMatch(head, clauses) => child(head) +: clauses.map(c => child(c.body))
      }
    }
  }

  object Node {
    def unapply(n: Node): Option[FunExpr] = Some(n.expr)
  }

  val rootNode = new Node(rootExpr, None)

  def allNodes = new Traversable[Node] {
    def foreach[U](f: Node => U) {
      def recurse(n: Node) {
        f(n)
        n.children.foreach(recurse(_))
      }
      recurse(rootNode)
    }
  }
}
