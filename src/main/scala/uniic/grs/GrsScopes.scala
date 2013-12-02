package uniic.grs

import uniic.misc._

class GrsScopes(grs: Grs) {

  lazy val allVarNodes: Set[GrsNodeOf[GrsVar]] = grs.allNodes.flatMap {
    case node@GrsNode(GrsVar(_)) => Some(node.downcast[GrsVar])
    case _ => None
  }.toSet

  /** Returns the node that declares the given var node. */
  def declarationSiteOf(varNode: GrsNodeOf[GrsVar]): Option[GrsNode] = Cache.declarationSiteOf.get(varNode)

  /** Returns all variables declared by the given node. */
  def varsDeclaredBy(node: GrsNode): Set[GrsNodeOf[GrsVar]] = Cache.varsDeclaredBy(node)

  /** Returns all variables that are in scope at the given edge. */
  def varsInScope(edge: GrsEdge): Set[GrsNodeOf[GrsVar]] = Cache.varsInScope(edge)

  /** Returns all vars under `node` that have a "using path" from `node`.
    *
    * A "using path" is a path with no metadata edges nor the node declaring the variable. */
  def usedFreeVarsBelow(node: GrsNode): Set[GrsNodeOf[GrsVar]] = Cache.usedFreeVarsBelow(node)

  private object Cache {
    lazy val declarationSiteOf: Map[GrsNodeOf[GrsVar], GrsNode] = {
      allVarNodes.flatMap(vn => findDeclarationSiteOf(vn).map(vn -> _)).toMap
    }

    def findDeclarationSiteOf(varNode: GrsNodeOf[GrsVar]): Option[GrsNode] = {
      var visited = Set.empty[GrsNode]
      def searchUpwards(edge: GrsEdge): Option[GrsEdge] = {
        edge.from.value match {
          case GrsCase if edge.childIndex == 0 => Some(edge)
          case GrsLambda(params) if edge.childIndex == 0 => Some(edge)
          case _ => searchAbove(edge.from)
        }
      }

      def searchAbove(node: GrsNode): Option[GrsEdge] = {
        if (!visited(node)) {
          visited += node
          val results = (grs.parentEdgesOf(node).flatMap(searchUpwards).toSet).toSeq
          results match {
            case Seq() => None
            case Seq(x) => Some(x)
            case ds => throw new CompilerException(
              "Grs appears to have more than one declaration site for " + varNode +
                ": " + ds.mkString(", ")
            )
          }
        } else {
          None
        }
      }

      searchAbove(varNode).map(_.from)
    }

    lazy val varsDeclaredBy: Map[GrsNode, Set[GrsNodeOf[GrsVar]]] = {
      MultiMap(declarationSiteOf.toSeq.map(_.swap): _*).toImmutable.withDefaultValue(Set.empty)
    }

    lazy val varsInScope: Map[GrsEdge, Set[GrsNodeOf[GrsVar]]] = {
      var result = MultiMap.empty[GrsEdge, GrsNodeOf[GrsVar]]
      var varsSeen = Set.empty[GrsNodeOf[GrsVar]]
      for ((varNode, declNode) <- declarationSiteOf) {
        varsSeen += varNode
        for (e <- grs.subgraphOfNode(declNode, true)) {
          result.addBinding(e, varNode)
        }
      }
      for (varNode <- allVarNodes if !varsSeen(varNode); e <- grs.allEdges) {
        result.addBinding(e, varNode)
      }
      result.toImmutable.withDefaultValue(Set.empty)
    }

    lazy val usedFreeVarsBelow: Map[GrsNode, Set[GrsNodeOf[GrsVar]]] = {
      grs.allNodes.map { startNode =>
        var vars = Set.empty[GrsNodeOf[GrsVar]]
        var visited = Set.empty[GrsNode]
        def visit(n: GrsNode, varsInScope: Set[GrsNodeOf[GrsVar]]) {
          if (!visited(n)) {
            visited += n
            n.tryDowncast[GrsVar].filter(!varsInScope(_)).foreach(vars += _)
            val newVarsInScope = varsInScope ++ varsDeclaredBy(n)
            n.children.foreach(visit(_, newVarsInScope))
          }
        }

        visit(startNode, Set.empty)
        startNode -> vars
      }.toMap
    }
  }

  lazy val freeTypeLevelVars: Set[String] = {
    grs.allNodes.flatMap(_.value.freeTypeLevelVars).toSet
  }
}
