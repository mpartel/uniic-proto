package uniic.grs

import scala.collection.mutable
import uniic.misc._

/** Does a reference count on a TGRS graph.
  *
  * The reference counting algorithm is based on
  * [1] "Conventional and Uniqueness Typing Systems in Graph Rewrite Systems" (1993)
  * by Barendsen and Smetsers
  *
  * The same ideas are also explained in
  * [2] "Guaranteeing Safe Destructive Updates through a Type System with Uniqueness Information for Graphs" (1994)
  * by Smetsers, Barendsen, Eekelen and Plasmeijer, but [1] is more detailed and recommended.
  * Note that example 4.7 in [2] is wrong. Look at the left picture of example 5.10 in [1] instead.
  *
  * Idea:
  * Let n be some node in the expression graph.
  * Now generally n is shared if there are at least two *distinct* paths to it from some ancestor node a.
  * Having two paths means that the evaluation of the graph may see n twice.
  *
  * There is one exception: Some composite expressions don't evaluate all of their subexpressions.
  * For instance, the if expression evaluates either its then part or
  * its else part but not both. This means we can improve the reference count by
  * ignoring path pairs whose first edges point to alternate execution paths.
  *
  * A second exception is possible: The algorithm in [1] also marks the 2nd and 3rd x in
  * `if x then x else x` as unshared, because it knows that evaluating the 1st x eliminates it.
  * We don't need this feature, so this implementation does not have it. It could be added
  * by marking edges instead of nodes, and filtering paths pairs where the first edge of one
  * is evaluated strictly before the first edge of the other.
  *
  * It is worth noting that both of the above exceptions are safe only as long as
  * the language does not speculatively evaluate conditional branches. If such speculative
  * evaluation were desired, it would have to be type-driven to be safe.
  *
  * Pseudo-code:
  * for each node n:
  *   for each edge m -(im)> n:
  *     for each ancestor a of m (including a = m):
  *       find a path p1 = a -(i1)> ... -> m -(im)> n
  *       find a path p2 = a -(i2)> ... -> n
  *         where
  *           - p2 is disjoint from p1 (except for a) and
  *           - (a, i1) and (a, i2) are not alternates
  *       if such a p1 and p2 could be found then mark n as shared
  *     if n was not marked, mark it as unique
  *
  * The reason for taking a parent `m` of `n` is to avoid the trivial path from `n` to `n`
  * but to find a cycle `n ~> m -> n` if one exists.
  */
class GrsRefCounts(grs: Grs) extends ExceptionContexts {
  def forNode(node: GrsNode): RefCount = if (isSharedNode(node)) SharedRef else UnsharedRef

  private val isSharedNode: Set[GrsNode] = {
    for {
      n <- grs.allNodes
      GrsEdge(m, im) <- parentEdgesOf(n) if firstPathEdgeConstraints(GrsEdge(m, im))
      a <- ancestorsOf(m)
      p1 <- findPath(a, m, firstPathEdgeConstraints).map(_ :+ GrsEdge(m, im))
      p2 <- findPath(a, n, secondPathEdgeConstraints(p1))
    } yield {
      verboseComment(s"n = $n; m = $m; a = $a")
      verboseComment(s"p1 = $p1")
      verboseComment(s"p2 = $p2")
      assert(
        p2.isEmpty || p1.nodes.tail.init.toSet.intersect(p2.nodes.tail.init.toSet).isEmpty,
        "p1 and p2 not disjoint!"
      )
      n
    }
  }.toSet

  private def parentEdgesOf(n: GrsNode): Iterable[GrsEdge] = {
    grs.parentsOf(n).flatMap { p =>
      p.edges.filter(e => e.to == n)
    }
  }

  private def ancestorsOf(start: GrsNode): Iterable[GrsNode] = {
    var visited = Set.empty[GrsNode]
    def visit(n: GrsNode) {
      if (!visited(n)) {
        visited += n
        grs.parentsOf(n).foreach(visit)
      }
    }
    visit(start)
    visited
  }

  private def findPath(from: GrsNode, to: GrsNode): Option[GrsPath] = {
    findPath(from, to, _ => true)
  }

  private def findPath(from: GrsNode, to: GrsNode, allowEdge: (GrsEdge => Boolean)): Option[GrsPath] = {
    var visited = Set.empty[GrsNode]

    def visit(n: GrsNode, pathHere: GrsPath): Option[GrsPath] = {
      if (n == to) {
        Some(pathHere)
      } else if (!visited(n)) {
        visited += n

        for (i <- n.children.indices) {
          val c = n.children(i)
          val newEdge = GrsEdge(n, i)
          if (allowEdge(newEdge)) {
            visit(c, pathHere :+ GrsEdge(n, i)) match {
              case Some(result) => {
                verboseComment(s"Found a path: $result")
                assert(!result.isEmpty)
                assert(result.nodes.head == from)
                assert(result.nodes.last == to)
                return Some(result)
              }
              case None =>
            }
          } else {
            verboseComment(s"Disallowed edge: $newEdge")
          }
        }

        None
      } else {
        None
      }
    }

    verboseContext(s"searching for a path $from ~> $to") {
      visit(from, GrsPath.empty)
    }
  }

  private def firstPathEdgeConstraints(edge: GrsEdge) = !edge.isMetadataEdge

  private def secondPathEdgeConstraints(p1: GrsPath): (GrsEdge => Boolean) = {
    val p1FirstEdge = p1.edges.head
    val p1LastEdge = p1.edges.last
    val disallowedDestNode = p1.nodes.toSet - p1LastEdge.to
    val disallowedEdge = p1.edges.toSet

    def check(edge: GrsEdge) = {
      if (disallowedEdge(edge) || disallowedDestNode(edge.to) || edge.isMetadataEdge) {
        false
      } else if (edge.from == p1FirstEdge.from) {
        val i1 = p1FirstEdge.childIndex
        val i2 = edge.childIndex
        !areAlternateExecutionPaths(edge.from, i1, i2)
      } else {
        true
      }
    }

    check
  }

  private def areAlternateExecutionPaths(n: GrsNode, i1: Int, i2: Int) = {
    if (i1 != i2) {
      n.value match {
        case GrsIfThenElse => i1 >= 1 && i2 >= 1
        case GrsMatch(_) => i1 >= 1 && i2 >= 1
        case _ => false
      }
    } else {
      false
    }
  }

  def toGraphviz = {
    val (gg, nodeMap) = grs.toGraphvizAndNodeMap
    for (n <- grs.allNodes) {
      val ggNode = gg.findNode(n).get
      ggNode.attributes("label") = ggNode.attributes("label") + " " + forNode(n)
    }
    gg
  }
}
