package uniic.flow
import uniic.imp._
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class FlowReachingDefinitionsAnalysisTest extends FreeSpec with ShouldMatchers {
  implicit def strToLabel(s: String) = FlowLabel(s)

  "Dragon Book example" in {
    def mkBlock(lbl: FlowLabel, jump: FlowJumpStmt, assignments: String*) = {
      val setStatements = assignments.map(v => FlowSetConst(FlowVar(v, 0), ImpInt(0)))
      lbl -> FlowBlock(Seq.empty, setStatements, jump)
    }
    val b1 = mkBlock("B1", FlowGoto("B2"), "i", "j", "a")
    val b2 = mkBlock("B2", FlowGotoCond(FlowVar("p", 0), "B3", "B4"), "i", "j")
    val b3 = mkBlock("B3", FlowGoto("B4"), "a")
    val b4 = mkBlock("B4", FlowGotoCond(FlowVar("p", 0), "B2", "ReturnBlock"), "i")
    val returnBlock = mkBlock("ReturnBlock", FlowReturn(FlowVar("a", 0)))
    val flowGraph = new FlowGraph(Seq(b1, b2, b3, b4, returnBlock), "B1")

    val analysis = new FlowReachingDefinitionsAnalysis(flowGraph)

    def check(lbl: String, expected: (String, String, Int)*) {
      val expectedSet = expected.map { case (v, l, i) => FlowVar(v, 0) -> (FlowStmtAddr(l, i)) }.toSet
      analysis.result.blockOutputs(lbl) should be (expectedSet)
    }

    check("B1", ("i", "B1", 0), ("j", "B1", 1), ("a", "B1", 2))
    check("B2", ("a", "B1", 2), ("a", "B3", 0), ("i", "B2", 0), ("j", "B2", 1))
    check("B3", ("a", "B3", 0), ("i", "B2", 0), ("j", "B2", 1))
    check("B4", ("i", "B4", 0), ("j", "B2", 1), ("a", "B1", 2), ("a", "B3", 0))
    check("ReturnBlock", ("i", "B4", 0), ("j", "B2", 1), ("a", "B1", 2), ("a", "B3", 0))
  }
}