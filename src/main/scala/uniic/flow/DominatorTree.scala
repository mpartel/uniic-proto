package uniic.flow
import uniic.misc.graphviz._
import scala.collection.mutable

/** The dominator tree of a flow graph.
  *
  * A basic block A dominates another B if all paths from the initial node to B must go through A.
  * The dominator A of B that is nearest to B on the path from the root to B is B's immediate dominator.
  * In a dominator tree, a node parent is its immediate dominator.
  *
  * See also: the dragon book, section 9.6.1. */
class DominatorTree private (flowGraph: FlowGraph, dominatorSets: Map[FlowLabel, Set[FlowLabel]]) {
  val parent: Map[FlowLabel, FlowLabel] = {
    flowGraph.allLabels
      .filter(_ != FlowGraph.entry)
      .map(lbl => lbl -> findImmediateDominator(lbl))
      .toMap
  }

  /** The ancestors of a node in the dominator tree, closest first. */
  def ancestors(lbl: FlowLabel): Seq[FlowLabel] = {
    parent.get(lbl).toSeq.flatMap { p: FlowLabel => p +: ancestors(p) }
  }

  def dominates(a: FlowLabel, b: FlowLabel) = dominatorSets(b).contains(a)
  def strictlyDominates(a: FlowLabel, b: FlowLabel) = a != b && dominates(a, b)

  private def findImmediateDominator(lbl: FlowLabel): FlowLabel = {
    // Breadth-first search through predecessors
    val queue: mutable.Queue[FlowLabel] = mutable.Queue(flowGraph.predecessors(lbl).toSeq: _*)
    val visited: mutable.Set[FlowLabel] = mutable.Set(queue: _*)
    while (!queue.isEmpty) {
      val pred = queue.dequeue()
      if (pred != lbl && dominates(pred, lbl)) {
        return pred
      } else {
        val newPreds = flowGraph.predecessors(pred).toSeq.filterNot(visited)
        visited ++= newPreds
        queue.enqueue(newPreds: _*)
      }
    }
    throw new IllegalStateException("Node " + lbl + " has no dominator")
  }

  val children: Map[FlowLabel, Set[FlowLabel]] = {
    var result = Map.empty[FlowLabel, Set[FlowLabel]]
    result ++= (flowGraph.allLabels.map(lbl => lbl -> Set.empty[FlowLabel]))
    flowGraph.allLabels.filter(_ != FlowGraph.entry).map { label =>
      val prevSet = result.getOrElse(parent(label), Set.empty)
      result += (parent(label) -> (prevSet + label))
    }
    result
  }

  def descendants(lbl: FlowLabel): Set[FlowLabel] = {
    children(lbl).flatMap { child => descendants(child) + child }
  }

  /** The dominance frontier of A is the set of blocks that are not strictly dominated by A and
    * are successors of A-dominated blocks.
    *
    * We say "strictly dominated" because a node may be in its own dominance frontier. */
  val dominanceFrontier: Map[FlowLabel, Set[FlowLabel]] = {
    flowGraph.allLabels.map { lbl =>
      lbl -> (this.descendants(lbl) + lbl).flatMap { desc =>
        flowGraph.successors(desc).filterNot(s => strictlyDominates(lbl, s))
      }
    }.toMap
  }

  def toGraphviz: GraphvizGraph = {
    val gg = new GraphvizGraph
    var nodeMap = Map.empty[FlowLabel, GraphvizGraph.Node]
    for (lbl <- flowGraph.allLabels) {
      val node = gg.addNode(lbl.name)
      nodeMap += (lbl -> node)
    }
    for (lbl <- flowGraph.allLabels.filterNot(_ == FlowGraph.exit); child <- children(lbl)) {
      gg.addEdge(nodeMap(lbl), nodeMap(child))
    }
    gg
  }
}

object DominatorTree {
  def apply(flowGraph: FlowGraph): DominatorTree = {
    val algo = new DominatorSetAlgorithm(flowGraph)
    new DominatorTree(flowGraph, algo.result.blockOutputs)
  }

  /** Based on the dragon book, section 9.6.1. */
  private class DominatorSetAlgorithm(val flowGraph: FlowGraph) extends ForwardsFlowAlgorithm {
    type Value = Set[FlowLabel]
    val outInit = flowGraph.allLabels.toSet
    val entryInit = Set(FlowGraph.entry)
    def transfer(lbl: FlowLabel, block: Option[FlowBlock], in: Set[FlowLabel]) = in + lbl
    def meet(left: Set[FlowLabel], right: Set[FlowLabel]) = left intersect right
  }
}