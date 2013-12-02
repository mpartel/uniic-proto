package uniic.flow

/** The location of a statement, identified by the block label
  * and the index to `FlowBlock#allStmts`.*/
case class FlowStmtAddr(lbl: FlowLabel, stmtIndex: Int)
