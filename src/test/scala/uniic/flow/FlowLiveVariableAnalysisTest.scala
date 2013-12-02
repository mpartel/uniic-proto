package uniic.flow

import uniic.imp._
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test.FlowTestUtils

class FlowLiveVariableAnalysisTest
  extends FreeSpec
  with ShouldMatchers
  with FlowTestUtils
{
  import FlowLiveVariableAnalysis._

  def lv(v: String) = SimpleLiveVar(FlowVar(v, 0))
  def lvPhi(vars: String*) = PhiLiveVar(vars.map(FlowVar(_, 0)))

  def checkIn(lbl: String, expected: LiveVar*)(implicit analysis: FlowLiveVariableAnalysis) {
    analysis.result.blockInputs(lbl) should be (expected.toSet)
  }

  "simple case with overwrite two blocks away" in {
    val flowGraph = mkFlowGraph(
      "B1" -> (
        BlockBuilder
        setConst("a")
        jump("B2")
      ),
      "B2" -> (
        BlockBuilder
        makeTuple("b", "a")
        jump("B3")
      ),
      "B3" -> (
        BlockBuilder
        setConst("a")
        ret("a")
      )
    )

    implicit val analysis = new FlowLiveVariableAnalysis(flowGraph)

    checkIn("B2", lv("a"))
    checkIn("B3")
  }

  "simple case with cycle" in {
    val flowGraph = mkFlowGraph(
      "B1" -> (
        BlockBuilder
        setConst("a")
        setConst("b")
        makeTuple("x", "b")
        makeTuple("y", "c")
        jump("b", "B2", "B1")
      ),
      "B2" -> (
        BlockBuilder
        makeTuple("x", "a")
        setConst("b")
        setConst("c")
        jump("d", "B3", "B1")
      ),
      "B3" -> (
        BlockBuilder
        ret("d")
      )
    )

    implicit val analysis = new FlowLiveVariableAnalysis(flowGraph)

    checkIn("B1", lv("c"), lv("d"))
    checkIn("B2", lv("a"), lv("d"))
    checkIn("B3", lv("d"))
  }

  "phis" in {
    val flowGraph = mkFlowGraph(
      "B1" -> (
        BlockBuilder
        setConst("a")
        jump("a", "B2", "B3")
      ),
      "B2" -> (
        BlockBuilder
        setConst("b")
        jump("B4")
      ),
      "B3" -> (
        BlockBuilder
        setConst("c")
        jump("B4")
      ),
      "B4" -> (
        BlockBuilder
        phi("d", "b", "c")
        ret("d")
      )
    )

    implicit val analysis = new FlowLiveVariableAnalysis(flowGraph)

    checkIn("B4", lvPhi("b", "c"))
    checkIn("B1")
  }
}
