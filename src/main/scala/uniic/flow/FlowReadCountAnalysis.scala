package uniic.flow

/** Finds how many times each variable may be read after a given point before being reassigned.
  *
  * Must be performed on an SSA.
  */
class FlowReadCountAnalysis(val flowGraph: FlowGraph) extends BackwardsFlowAlgorithm {
  import FlowReadCountAnalysis._

  type Value = Map[FlowVar, ReadCount]

  val inInit = Map.empty[FlowVar, ReadCount].withDefaultValue(NotRead)
  val exitInit = inInit

  def transfer(lbl: FlowLabel, block: Option[FlowBlock], in: Value) = {
    val allStmts = block.map(_.allStmts).getOrElse(Seq.empty)
    allStmts.foldRight(in)(transferStmt(_, _))
  }

  def transferStmt(stmt: FlowStmt, in: Value): Value = {
    transferStmtReads(stmt, transferStmtWrites(stmt, in))
  }

  def transferStmtReads(stmt: FlowStmt, in: Value): Value = {
    stmt match {
      case FlowPhiStmt(to, from) => {
        // In an SSA with no redundant phis, a phi read is the last read of a variable
        // We assume here we've built the SSA correctly with no redundant phis.
        from.foldLeft(in) { (m, v) => m + (v -> NotRead) }
      }
      case _ => stmt.readVars.foldLeft(in) { (m, v) => m + (v -> (m(v).increment)) }
    }
  }

  def transferStmtWrites(stmt: FlowStmt, in: Value): Value = {
    stmt.resultVars.foldLeft(in) { (m, v) => m + (v -> NotRead) }
  }

  def meet(left: Value, right: Value) = {
    val vars = left.keySet ++ right.keySet
    vars.map { v =>
      v -> (left(v) max right(v))
    }.toMap.withDefaultValue(NotRead)
  }

  def after(addr: FlowStmtAddr): Value = {
    val in = result.blockOutputs(addr.lbl)
    val relevantStmts = flowGraph.blocks(addr.lbl).allStmts.drop(addr.stmtIndex + 1)
    relevantStmts.foldRight(in)(transferStmt(_, _))
  }
}

object FlowReadCountAnalysis {
  sealed abstract class ReadCount(private val num: Int) extends Ordered[ReadCount] {
    def increment: ReadCount
    def compare(that: ReadCount) = this.num - that.num
    def max(that: ReadCount) = if (this.num >= that.num) this else that
  }
  case object NotRead extends ReadCount(0) {
    def increment = ReadOnce
  }
  case object ReadOnce extends ReadCount(1) {
    def increment = ReadMoreThanOnce
  }
  case object ReadMoreThanOnce extends ReadCount(2) {
    def increment = this
  }
}
