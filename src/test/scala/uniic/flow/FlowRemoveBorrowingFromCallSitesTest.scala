package uniic.flow

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test._
import uniic.types.MBorrowed

class FlowRemoveBorrowingFromCallSitesTest
  extends FreeSpec
  with ShouldMatchers
  with FlowTestUtils
{
  "one borrowing site in one block" in {
    val graph = mkFlowGraph(
      FlowLabel("main"),
      FlowCall('$unused, 'f, Seq('a @@ 'a)),
      FlowReturn('a)
    )

    val result = FlowRemoveBorrowingFromCallSites(graph)

    result.allStmtsWithLabelsSeq should be (equivalentTo (Seq(
      FlowLabel("main"),
      FlowBorrowingRemoval('f$nb, 'f),
      FlowCall('f$nbr, 'f$nb, Seq('a)),
      FlowSplitTuple(Seq('$unused, 'a), 'f$nbr),
      FlowReturn('a)
    )))
  }

  "two borrowing sites in one block" in {
    val graph = mkFlowGraph(
      FlowLabel("main"),
      FlowCall('$unused, 'f, Seq('a @@ 'a)),
      FlowCall('$unused2, 'f, Seq('a @@ 'a)),
      FlowReturn('a)
    )

    val result = FlowRemoveBorrowingFromCallSites(graph)

    result.allStmtsWithLabelsSeq should be (equivalentTo (Seq(
      FlowLabel("main"),
      FlowBorrowingRemoval('f$nb, 'f),
      FlowCall('f$nbr, 'f$nb, Seq('a)),
      FlowSplitTuple(Seq('$unused, 'a), 'f$nbr),
      FlowBorrowingRemoval('f$nb, 'f),
      FlowCall('f$nbr, 'f$nb, Seq('a)),
      FlowSplitTuple(Seq('$unused2, 'a), 'f$nbr),
      FlowReturn('a)
    )))
  }

  "multiple blocks" in {
    val graph = mkFlowGraph(
      FlowLabel("main"),
      FlowCall('cond, 'f, Seq('a @@ 'a)),
      FlowGotoCond('cond, FlowLabel("left"), FlowLabel("right")),

      FlowLabel("left"),
      FlowCall('$unused, 'f, Seq('a @@ 'a)),
      FlowGoto("main"),

      FlowLabel("right"),
      FlowCall('$unused2, 'f, Seq('a @@ 'a)),
      FlowReturn('a)
    )

    val result = FlowRemoveBorrowingFromCallSites(graph)

    result.allStmtsWithLabelsSeq should be (equivalentTo (Seq(
      FlowLabel("main"),
      FlowBorrowingRemoval('f$nb, 'f),
      FlowCall('f$nbr, 'f$nb, Seq('a)),
      FlowSplitTuple(Seq('cond, 'a), 'f$nbr),
      FlowGotoCond('cond, FlowLabel("left"), FlowLabel("right")),

      FlowLabel("left"),
      FlowBorrowingRemoval('f$nb, 'f),
      FlowCall('f$nbr, 'f$nb, Seq('a)),
      FlowSplitTuple(Seq('$unused, 'a), 'f$nbr),
      FlowGoto("main"),

      FlowLabel("right"),
      FlowBorrowingRemoval('f$nb, 'f),
      FlowCall('f$nbr, 'f$nb, Seq('a)),
      FlowSplitTuple(Seq('$unused2, 'a), 'f$nbr),
      FlowReturn('a)
    )))
  }
}
