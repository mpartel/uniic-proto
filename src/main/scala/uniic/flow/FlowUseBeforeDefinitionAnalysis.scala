package uniic.flow
import uniic.misc._

/** Finds uses of a variable before its definition. Run on a plain (non-SSA) flowgraph. */
object FlowUseBeforeDefinitionAnalysis {
  def check(flowGraph: FlowGraph, predefinedVars: Set[FlowVar]) {
    val allUses = apply(flowGraph, predefinedVars)
    if (!allUses.isEmpty) {
      val usesByLabel = allUses.groupBy(_._2)
      val errors = usesByLabel.map { case (lbl, uses) =>
        Inflections.pluralWord(uses.size, "Variable") + " " +
          uses.toSeq.map(u => s"`${u._1}`").sorted.mkString(", ") +
          " potentially used in " + lbl + " before being defined"
      }
      throw new CompilerException(errors.mkString("\n"))
    }
  }

  def apply(flowGraph: FlowGraph, predefinedVars: Set[FlowVar]): Set[(FlowVar, FlowLabel)] = {
    val allVars = flowGraph.blocks.flatMap(_._2.allDefinedVars)

    // Using the reaching definitions with dummy definitions trick described in the Dragon Book, section 9.2.4.
    val dummyDefs = allVars.map(v => (v -> FlowStmtAddr(FlowGraph.entry, 0))).toSet
    val analysis = new FlowReachingDefinitionsAnalysis(flowGraph, dummyDefs)

    analysis.result.blockInputs.flatMap { case (lbl, defs) =>
      val varsRequired = flowGraph.varsRequired.getOrElse(lbl, Set.empty)
      val dummyDefsHere = defs.intersect(dummyDefs)
      val usedDummyDefsHere = dummyDefsHere.filter { case (v, d) =>
        varsRequired.contains(v) && !predefinedVars.contains(v)
      }
      usedDummyDefsHere.map(_._1 -> lbl)
    }.toSet
  }
}