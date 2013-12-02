package uniic.fun.analysis
import uniic.fun._

private[analysis] trait FunTreeBase {
  type Node <: NodeBase

  val rootExpr: FunExpr
  val rootNode: Node

  trait NodeBase {
    self: Node =>
    val expr: FunExpr
    val parent: Option[Node]
    def children: Seq[Node]

    def childEdges = children.indices.map(Edge(this, _))

    lazy val parentEdge: Option[Edge] = parent.map { p =>
      val i = p.children.indexOf(this)
      assert(i != -1)
      Edge(p, i)
    }

    lazy val descendants: Stream[Node] = Stream(children: _*) ++ children.flatMap(_.descendants)

    override def toString = "(" + FunExprToShortString(expr) + ")@" + super.hashCode
  }

  case class Edge(from: Node, index: Int) {
    def to = from.children(index)
  }
}