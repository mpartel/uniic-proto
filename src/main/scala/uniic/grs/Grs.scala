package uniic.grs

import scala.collection.mutable
import uniic.misc._
import uniic.misc.graphviz.GraphvizGraph

/** Graph rewrite system.
  *
  * Note: the proper usage of this trait is `new { val rootNode = ... } with Grs`
  * in order to get initialization order right. */
trait Grs {
  import Grs._

  val rootNode: GrsNodeOf[GrsRoot.type]
  val rootEdge = rootNode.soleEdge

  /** It is sometimes convenient to treat the indexes of this as the "names" of the nodes.
    *
    * allNodes is not lazy because some nodes may be constructed lazily and we want to
    * force them here. */
  val allNodes: IndexedSeq[GrsNode] = {
    val ns = rootNode +: subgraphOfEdge(rootEdge, true).map(_.to)
    CollectionUtil.uniq(ns).toIndexedSeq
  }

  lazy val allEdges: Set[GrsEdge] = allNodes.flatMap(_.edges).toSet

  lazy val contains: (GrsNode => Boolean) = allNodes.toSet

  lazy val indexOf: (GrsNode => Int) = allNodes.zipWithIndex.toMap // An optimization and convenience over allNodes.indexOf

  /** Maps the index of a node to the indexes of its children */
  lazy val childIndexesOf: IndexedSeq[Seq[Int]] = {
    allNodes.indices.map(i => allNodes(i).children.map(indexOf))
  }

  /** Returns all immediate parent edges of a node. */
  lazy val parentEdgesOf: (GrsNode => Set[GrsEdge]) = {
    val result = MultiMap.empty[GrsNode, GrsEdge]

    var visited = Set.empty[GrsNode]
    def visit(p: GrsNode, i: Int, n: GrsNode) {
      result.addBinding(n, GrsEdge(p, i))
      if (!visited(n)) {
        visited += n
        n.children.zipWithIndex.foreach { case (c, i) => visit(n, i, c) }
      }
    }

    visit(rootNode, 0, rootNode.soleChild)

    result.toImmutable.withDefaultValue(Set.empty)
  }

  def parentsOf(node: GrsNode): Set[GrsNode] = parentEdgesOf(node).map(_.from)

  def ancestorsOf(node: GrsNode): Set[GrsNode] = {
    var found = Set.empty[GrsNode]
    var frontier = parentsOf(node)
    while (!frontier.isEmpty) {
      found ++= frontier
      frontier = frontier.flatMap(parentsOf).filterNot(found)
    }
    found
  }

  /** Returns all edges below the start edge, including the start edge. */
  def subgraphOfEdge(startEdge: GrsEdge, includeMetadataEdges: Boolean): IndexedSeq[GrsEdge] = {
    var result = IndexedSeq.empty[GrsEdge]
    var visited = Set.empty[GrsEdge]
    def visit(e: GrsEdge) {
      if (!visited(e) && (includeMetadataEdges || !e.isMetadataEdge)) {
        visited += e
        result :+= e
        e.to.edges.foreach(visit)
      }
    }
    visit(startEdge)
    result
  }

  /** Returns all edges below the given start node, not including the start node. */
  def subgraphOfNode(startNode: GrsNode, includeMetadataEdges: Boolean): Seq[GrsEdge] = {
    startNode.edges.flatMap(subgraphOfEdge(_, includeMetadataEdges))
  }

  def descendantsOf(startNode: GrsNode, includeMetadataEdges: Boolean): Seq[GrsNode] = {
    subgraphOfNode(startNode, includeMetadataEdges).map(_.to)
  }

  def withRedirect(oldNode: GrsNode, newNode: GrsNode): Grs = {
    withRedirects(Map(oldNode -> newNode))
  }

  def withRedirects(map: Map[GrsNode, GrsNode]): Grs = {
    val newNodes = map.values.filterNot(this.contains).toSeq

    if (!newNodes.isEmpty) {
      withSubtree(GrsTuple(newNodes.size).toNode(newNodes)).withRedirects(map)
    } else {
      val changedParents: Iterable[GrsNode] = map.keys.flatMap(parentsOf(_))
      val changes: Map[Int, Seq[Int]] = changedParents.map { p =>
        val newChildIndexes = p.children.map { pc => indexOf(map.getOrElse(pc, pc)) }
        indexOf(p) -> newChildIndexes
      }.toMap

      rewireNodes(changes)
    }
  }

  def rewireNodes(changedChildIndexes: Map[Int, Seq[Int]]) = {
    lazy val newNodes: IndexedSeq[GrsNode] = allNodes.map { n =>
      n.lazyCloneWithChildren { () =>
        val i = indexOf(n)
        changedChildIndexes.getOrElse(i, childIndexesOf(i)).map(ci => newNodes(ci))
      }
    }

    val newRoot = newNodes(indexOf(rootNode)).downcast[GrsRoot.type]
    Grs(newRoot)
  }

  private def withSubtree(subtreeRoot: GrsNode): Grs = {
    val self = this
    val subtreeNodes = subtreeRoot +: subtreeRoot.edges.flatMap(subgraphOfEdge(_, true)).map(_.to)
    new {
      val rootNode = self.rootNode
      override val allNodes = self.allNodes ++ subtreeNodes
    } with Grs
  }

  lazy val refCounts = new GrsRefCounts(this)

  lazy val scopes = new GrsScopes(this)

  def isEquvalentTo(that: Grs) = {
    var indexEquiv = Map.empty[Int, Int]
    def compare(ai: Int, bi: Int): Boolean = {
      indexEquiv.get(ai) match {
        case None => {
          indexEquiv += (ai -> bi)
          if (this.allNodes.indices.contains(ai) &&
              that.allNodes.indices.contains(bi)) {
            val a = this.allNodes(ai)
            val b = that.allNodes(bi)
            val chiA = this.childIndexesOf(ai)
            val chiB = that.childIndexesOf(bi)
            if (a.value == b.value && chiA.length == chiB.length) {
              chiA.zip(chiB).forall { case (ca, cb) =>
                indexEquiv.get(ca) match {
                  case Some(x) if x != cb => false
                  case _ => compare(ca, cb)
                }
              }
            } else {
              false
            }
          } else {
            false
          }
        }
        case Some(x) => x == bi
      }
    }

    compare(0, 0)
  }

  override def toString = {
    val nodeStrs = allNodes.indices.map(i => i + ": " + allNodes(i).value)
    val edgeStrs = childIndexesOf.indices.map { i =>
      val js = childIndexesOf(i)
      i + " -> " + js.mkString("{", ", ", "}")
    }
    "Grs(\n  " + nodeStrs.mkString("\n  ") + "\n  " + edgeStrs.mkString("\n  ") + "\n}"
  }

  def toGraphviz: GraphvizGraph = toGraphvizAndNodeMap._1

  def toGraphvizAndNodeMap: (GraphvizGraph, Map[GrsNode, GraphvizGraph.Node]) = {
    val gg = new GraphvizGraph
    var nodeMap = mutable.Map.empty[GrsNode, GraphvizGraph.Node]
    for (n <- allNodes) {
      val ggNode = gg.addNode(n.value.toString, "shape" -> "rectangle")
      ggNode.key = n
      nodeMap.update(n, ggNode)
    }
    for (n <- allNodes; (c, i) <- n.children.zipWithIndex) {
      val ggEdge = gg.addEdge(nodeMap(n), nodeMap(c), "label" -> i.toString)
      if (n.value.isMetadataEdge(i)) {
        ggEdge.attributes("style") = "dashed"
      }
      ggEdge.key = GrsEdge(n, i)
    }
    (gg, nodeMap.toMap)
  }
}

object Grs {
  def apply(init: GrsNode): Grs = {
    if (init.value == GrsRoot) {
      apply(GrsRoot.toNode(init.children))
    } else {
      apply(GrsRoot.toNode(Seq(init)))
    }
  }

  def apply(_rootNode: GrsNodeOf[GrsRoot.type]): Grs = {
    //TODO: investigate DelayedInit
    new { val rootNode = _rootNode } with Grs
  }
}
