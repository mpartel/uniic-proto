package uniic.fun.analysis
import uniic.misc.graphviz._
import uniic.misc._
import uniic.fun._

trait FunTreeGraphviz extends FunTreeBase {
  type Node <: NodeBase

  trait GraphvizConf {
    def nodeLabel(n: Node): String = FunExprToShortString(n.expr)
    def edgeLabel(e: Edge): String = e.index.toString
    def nodeShape(n: Node): String = "rectangle"
  }

  object GraphvizConf {
    val default = new GraphvizConf {}
  }

  def toGraphviz: GraphvizGraph = toGraphviz(GraphvizConf.default)

  def toGraphviz(conf: GraphvizConf): GraphvizGraph = toGraphvizAndNodeMap(conf)._1

  def toGraphvizAndNodeMap(conf: GraphvizConf): (GraphvizGraph, Map[Node, GraphvizGraph.Node]) = {
    val gg = new GraphvizGraph

    var nodeMap = Map.empty[Node, GraphvizGraph.Node]
    def recurse(n: Node): GraphvizGraph.Node = {
      val gn = gg.addNode(conf.nodeLabel(n), "shape" -> conf.nodeShape(n))
      nodeMap += (n -> gn)

      for ((child, i) <- n.children.zipWithIndex) {
        val cgn = recurse(child)
        val edge = Edge(child, i)
        gg.addEdge(nodeMap(n), nodeMap(child), "label" -> conf.edgeLabel(edge))
      }
      gn
    }
    recurse(rootNode)

    (gg, nodeMap)
  }
}