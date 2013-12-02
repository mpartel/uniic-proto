package uniic.flow

import scala.collection.mutable
import uniic.misc._
import uniic.misc.graphviz.GraphvizGraph

/** The data flow framework as described in "Compilers: principles, techniques & tools" (Aho, Lam, Sethi, Ullman),
  * section 9.3 "Foundations of data-flow analysis". */
trait FlowAlgorithm {
  type Value

  val flowGraph: FlowGraph

  def processingOrder: Seq[FlowLabel]

  val maxIterations: Option[Int] = Some(2000)

  def meet(left: Value, right: Value): Value

  def result: Result

  trait Result {
    val blockOutputs: Map[FlowLabel, Value]
    val blockInputs: Map[FlowLabel, Value]
  }

  def toGraphviz: GraphvizGraph = {
    val gg = flowGraph.toGraphviz
    for (lbl <- flowGraph.allLabels; node <- gg.findNode(lbl)) {
      val text = node.attributes("label")
      val input = valueToGraphvizLabel(result.blockInputs(lbl))
      val output = valueToGraphvizLabel(result.blockOutputs(lbl))
      val newText = input + "\\n\\n" + text + "\\l" + output + "\\n"
      node.attributes("label") = newText
    }
    gg
  }

  def valueToGraphvizLabel(v: Value): String = v.toString.replace("\n", "\\n")
}

trait ForwardsFlowAlgorithm extends FlowAlgorithm {
  val outInit: Value
  val entryInit: Value

  lazy val processingOrder: Seq[FlowLabel] = flowGraph.allLabels.filterNot(_ == FlowGraph.entry)

  def transfer(lbl: FlowLabel, block: Option[FlowBlock], in: Value): Value

  lazy val result: Result = {
    val outs = mutable.Map.empty[FlowLabel, Value]
    val ins = mutable.Map.empty[FlowLabel, Value]

    outs.update(FlowGraph.entry, entryInit)
    for ((lbl, block) <- flowGraph.blocks) {
      outs.update(lbl, outInit)
    }
    outs.update(FlowGraph.exit, outInit)

    var changed = true
    var iter = 0
    while (changed) {
      if (maxIterations.forall(iter > _)) {
        throw new CompilerException(
          s"Flow algorithm failed to converge in ${maxIterations.get} iterations"
        )
      }
      iter += 1

      changed = iterateOnce(outs, ins)
    }

    new Result {
      val blockOutputs = outs.toMap
      val blockInputs = ins.toMap
    }
  }

  protected[this] def iterateOnce(outs: mutable.Map[FlowLabel, Value], ins: mutable.Map[FlowLabel, Value]): Boolean = {
    var changed = false
    for (lbl <- processingOrder) {
      val block = flowGraph.blocks.get(lbl)
      val preds = flowGraph.predecessors(lbl)
      val in = preds.map(outs(_)).reduce(meet(_, _))
      val out = transfer(lbl, block, in)

      changed ||= (out != outs(lbl))

      ins.update(lbl, in)
      outs.update(lbl, out)
    }
    changed
  }
}

trait BackwardsFlowAlgorithm extends FlowAlgorithm {
  val inInit: Value
  val exitInit: Value

  lazy val processingOrder = flowGraph.allLabels.filterNot(_ == FlowGraph.exit)

  def transfer(lbl: FlowLabel, block: Option[FlowBlock], out: Value): Value

  lazy val result: Result = {
    val ins = mutable.Map.empty[FlowLabel, Value]
    val outs = mutable.Map.empty[FlowLabel, Value]

    ins.update(FlowGraph.exit, exitInit)
    for ((lbl, block) <- flowGraph.blocks) {
      ins.update(lbl, inInit)
    }
    ins.update(FlowGraph.entry, inInit)

    var changed = true
    var iter = 0
    while (changed) {
      if (maxIterations.forall(iter > _)) {
        throw new CompilerException(
          s"Flow algorithm failed to converge in ${maxIterations.get} iterations"
        )
      }
      iter += 1

      changed = iterateOnce(ins, outs)
    }

    new Result {
      val blockOutputs = outs.toMap
      val blockInputs = ins.toMap
    }
  }

  protected[this] def iterateOnce(ins: mutable.Map[FlowLabel, Value], outs: mutable.Map[FlowLabel, Value]): Boolean = {
    var changed = false
    for (lbl <- processingOrder) {
      val block = flowGraph.blocks.get(lbl)
      val succs = flowGraph.successors(lbl)
      val out = succs.map(ins(_)).reduce(meet(_, _))
      val in = transfer(lbl, block, out)

      changed ||= in != ins(lbl)

      ins.update(lbl, in)
      outs.update(lbl, out)
    }
    changed
  }
}
