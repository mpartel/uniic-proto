package uniic.flow

/** A basic block of a flow graph. */
case class FlowBlock(phis: Seq[FlowPhiStmt], basics: Seq[FlowBasicStmt], jump: FlowJumpStmt) {
  lazy val allStmts = (phis ++ basics) :+ jump

  /** All variables defined somewhere in this block. */
  lazy val allDefinedVars: Set[FlowVar] = allStmts.flatMap(_.resultVars).toSet

  /** The latest versions of variables defined by this block.
    *
    * That is, if the block first assigns x1 and then x2, then only x2 is returned. */
  lazy val latestLocallyDefinedVars: Map[String, FlowVar] = {
    var result = Map.empty[String, FlowVar]
    for (stmt <- allStmts.reverse; v <- stmt.resultVars) {
      if (!result.contains(v.baseName)) {
        result += (v.baseName -> v)
      }
    }
    result
  }
}

object FlowBlock {
  def fromStatements(input: Seq[FlowStmt]): FlowBlock = {
    FlowGraphParsers.parseBlockAfterLabel(input)
  }
}
