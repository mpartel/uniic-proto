package uniic.fun

import uniic.grs._
import uniic.misc._
import uniic.types._
import uniic.fun.tr._
import uniic.fun.analysis.FunTree
import scala.annotation.tailrec

object FunToGrs {
  def apply(tree: FunTree, symTab: Map[String, GrsValue]): Grs = {
    val internalSymTab = symTab.mapValues(ValueSymbol(_))
    val baseGrs = Grs(apply(ConstantSharing(tree).rootExpr, internalSymTab))
    val thunklessGrs = eliminateThunks(baseGrs)
    addClosures(thunklessGrs)
  }

  class CyclicLetException(msg: String) extends CompilerException(msg)

  /** A symbol in the symbol table can either refer to a node or a value.
    * Toplevel symbols refer to a value and a new node is created each time it
    * is looked up. This makes it possible to type toplevel functions polymorphically
    * in a function. */
  private sealed trait Symbol
  private case class NodeSymbol(node: GrsNode) extends Symbol
  private case class ValueSymbol(value: GrsValue) extends Symbol

  private type SymTab = Map[String, Symbol]
  private val SymTab = Map

  private def apply(funExpr: FunExpr, symTab: SymTab): GrsNode = {
    funExpr match {
      case FunUnit => GrsUnit.toNode
      case FunBool(v) => GrsBool(v).toNode
      case FunInt(v) => GrsInt(v).toNode
      case FunTupleExpr(members) => GrsTuple(members.length).toNode(members.map(apply(_, symTab)))
      case FunLambda(params, body) => {
        val paramInfo = params.map { case (x, te) => (x, GrsVar(x).toNode, te) }
        val paramEntries = paramInfo.map { case (x, n, _) => (x, NodeSymbol(n)) }
        val paramTypes = paramInfo.map(_._3)
        val paramNodes = paramInfo.map(_._2)
        val paramList = GrsParamList(paramNodes.length).toNode(paramNodes)

        val newSymTab = symTab ++ paramEntries
        val bodyNode = apply(body, newSymTab)

        val stubClosure = GrsUnused.toNode // Filled in by addClosures

        GrsLambda(paramTypes).toNode(Seq(paramList, stubClosure, bodyNode))
      }
      case FunBuiltinFunction(name, ts, paramNames, impl) => ???
      case FunBuiltinValue(value: Any, ty: Option[BuiltinType]) => GrsBuiltinValue(value, ty).toNode
      case FunVar(v) => {
        symTab.get(v) match {
          case Some(NodeSymbol(node)) => node
          case Some(ValueSymbol(v)) => v.toNode
          case None => GrsVar(v).toNode
        }
      }
      case FunApply(fun, args) => {
        GrsApply(args.size).toNode(apply(fun, symTab) +: args.map(apply(_, symTab)))
      }
      case FunLet(bindings, body) => {
        checkNoCyclicAliases(bindings)
        var newSymTab: SymTab = null
        val newSyms = bindings.map { case (name, expr) =>
          val thunkNode = GrsThunk.toNodeWithLazyChildren { () => Seq(apply(expr, newSymTab)) }
          name -> NodeSymbol(thunkNode)
        }
        newSymTab = symTab ++ newSyms
        apply(body, newSymTab)
      }
      case FunIfThenElse(cond, thenBody, elseBody) => {
        val condGrs = apply(cond, symTab)
        val thenGrs = apply(thenBody, symTab)
        val elseGrs = apply(elseBody, symTab)
        GrsIfThenElse.toNode(Seq(condGrs, thenGrs, elseGrs))
      }
      case FunMatch(head, clauses) => {
        val grsClauses = clauses.map { c =>
          val patternEntries = c.pattern.patternVars.map(v => v -> GrsVar(v).toNode)
          val bodySymTab = symTab ++ patternEntries.map { case (v, n) => v -> NodeSymbol(n) }
          val grsPattern = applyPattern(c.pattern, patternEntries.toMap)
          val grsBody = apply(c.body, bodySymTab)
          GrsCase.toNode(Seq(grsPattern, grsBody))
        }
        val grsHead = apply(head, symTab)
        GrsMatch(clauses.length).toNode(grsHead +: grsClauses)
      }
    }
  }

  private def applyPattern(pattern: Pattern, symTab: Map[String, GrsNodeOf[GrsVar]]): GrsNode = {
    pattern match {
      case PatUnit => GrsUnit.toNode
      case PatBool(v) => GrsBool(v).toNode
      case PatInt(v) => GrsInt(v).toNode
      case PatUnused => GrsUnused.toNode
      case PatVar(v) => symTab(v)
      case PatTuple(members) => GrsTuple(members.length).toNode(members.map(applyPattern(_, symTab)))
    }
  }

  private def checkNoCyclicAliases(bindings: Seq[(String, FunExpr)]) {
    val aliasMap: Map[String, String] = bindings.map {
      case (x, FunVar(y)) => Some(x -> y)
      case _ => None
    }.flatten.toMap

    for (x <- aliasMap.keys) {
      SimpleGraph.fromMap(aliasMap).findCycle(x) match {
        case Some(cycle) => {
          throw new CyclicLetException("Cycle in let binding: " + cycle.mkString("(", ", ", ")"))
        }
        case None =>
      }
    }
  }

  @tailrec
  private def addClosures(grs: Grs): Grs = {
    grs.allNodes.find {
      case node@GrsNode(_: GrsLambda) if node.children(1).value == GrsUnused => true
      case _ => false
    } match {
      case Some(lambdaNode) => {
        val externalNodes = findExternalNodes(grs, lambdaNode)
        val closureNode = GrsClosure(externalNodes.size).toNode(externalNodes.toSeq)
        val newGrs = grs.withRedirect(lambdaNode.children(1), closureNode)
        addClosures(newGrs)
      }
      case None => grs
    }
  }

  /** An external node is one that will go to a lambda's closure.
    * Roughly, it is a descendant of the lambda body that is reachable
    * from some node that is not a descendant of the lambda. */
  private def findExternalNodes(grs: Grs, lambdaNode: GrsNode): Seq[GrsNode] = {
    val descendants = subgraphOf(lambdaNode.children(2))
    val stopSet = descendants.toSet + lambdaNode
    val reachables = subgraphUntil(grs.rootNode, stopSet).toSet
    CollectionUtil.uniq(descendants.filter(reachables)) // We could use the descendant set but we want deterministic order
  }

  private def subgraphOf(initialNode: GrsNode): Seq[GrsNode] = subgraphUntil(initialNode, Set.empty)

  private def subgraphUntil(
    initialNode: GrsNode,
    stopAt: GrsNode => Boolean
  ): Seq[GrsNode] = {
    var visited: Set[GrsNode] = Set(initialNode)
    var result = List(initialNode) // Using List for deterministic order for the benefit of tests
    def visit(n: GrsNode) {
      if (!visited(n)) {
        visited += n
        result ::= n
        val stop = stopAt(n)
        if (!stop) {
          n.children.foreach(visit)
        }
      }
    }
    initialNode.children.foreach(visit)
    result.reverse
  }

  @tailrec
  private def eliminateThunks(grs: Grs): Grs = {
    grs.allNodes.find {
      case GrsNode(GrsThunk) => true
      case _ => false
    } match {
      case Some(node) => {
        if (node.children.head == node) {
          throw new CyclicLetException("Cycle detected when eliminating thunks")
        }
        eliminateThunks(grs.withRedirect(node, node.children.head))
      }
      case None => grs
    }
  }
}
