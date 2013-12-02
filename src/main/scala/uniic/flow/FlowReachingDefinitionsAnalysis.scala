package uniic.flow

/** The result is the set of (var, declSite) pairs that are visible to that location in the code. */
class FlowReachingDefinitionsAnalysis(
  val flowGraph: FlowGraph,
  val entryInit: Set[(FlowVar, FlowStmtAddr)] = Set.empty
) extends ForwardsFlowAlgorithm {
  type Value = Set[(FlowVar, FlowStmtAddr)]

  val outInit = Set.empty[(FlowVar, FlowStmtAddr)]

  def transfer(lbl: FlowLabel, maybeBlock: Option[FlowBlock], in: Value): Value = {
    maybeBlock match {
      case Some(block) => {
        val localDefs: Seq[(FlowVar, Int)] = block.allStmts.zipWithIndex.flatMap {
          case (stmt, i) => stmt.resultVars.map(_ -> i)
        }
        localDefs.foldLeft(in) { case (visibleDefSites, (declaredVar, i)) =>
          val otherVars = visibleDefSites.filter(_._1 != declaredVar)
          otherVars + (declaredVar -> FlowStmtAddr(lbl, i))
        }
      }
      case None => in
    }
  }

  def meet(left: Value, right: Value): Value = {
    left ++ right
  }
}
