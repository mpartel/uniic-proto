package uniic.flow

/** A variable is 'live' at some point if it may be read before it's written again. */
class FlowLiveVariableAnalysis(val flowGraph: FlowGraph) extends BackwardsFlowAlgorithm {
  import FlowLiveVariableAnalysis._

  type Value = Set[LiveVar]

  val inInit = Set.empty[LiveVar]
  val exitInit = Set.empty[LiveVar]

  def meet(left: Value, right: Value) = left union right

  def transfer(lbl: FlowLabel, maybeBlock: Option[FlowBlock], out: Value) = {
    maybeBlock match {
      case Some(block) => {
        block.allStmts.foldRight(out)(transferStmt(_, _))
      }
      case None => out
    }
  }

  def transferStmt(stmt: FlowStmt, out: Value): Value = {
    stmt match {
      case FlowPhiStmt(to, from) => eliminate(to, out + PhiLiveVar(from))
      case _ => {
        val withReads = out ++ stmt.readVars.map(SimpleLiveVar(_))
        stmt.resultVars.foldLeft(withReads) { case (set, rv) => eliminate(rv, set) }
      }
    }
  }

  def eliminate(v: FlowVar, from: Value): Value = {
    from filter {
      case SimpleLiveVar(lv) => v != lv
      case PhiLiveVar(lvs) => !lvs.contains(v)
    }
  }

  def after(addr: FlowStmtAddr): Value = {
    val in = result.blockOutputs(addr.lbl)
    val relevantStmts = flowGraph.blocks(addr.lbl).allStmts.drop(addr.stmtIndex + 1)
    relevantStmts.foldRight(in)(transferStmt(_, _))
  }
}

object FlowLiveVariableAnalysis {
  sealed trait LiveVar {
    def contains(v: FlowVar): Boolean
    def containsBaseName(baseName: String): Boolean
  }
  /** A simple live variable */
  case class SimpleLiveVar(lv: FlowVar) extends LiveVar {
    def contains(v: FlowVar) = v == lv
    def containsBaseName(baseName: String) = baseName == lv.baseName
  }
  /** A set of potentially live variables. Writing to one of them will eliminate the whole set. */
  case class PhiLiveVar(lvs: Seq[FlowVar]) extends LiveVar {
    def contains(v: FlowVar) = lvs.contains(v)
    def containsBaseName(baseName: String) = lvs.exists(_.baseName == baseName)
  }
}
