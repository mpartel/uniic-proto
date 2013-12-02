package uniic.flow
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class DominatorTreeTest extends FreeSpec with ShouldMatchers {
  "The case from 'SSA is functional programming'" in {
    def lbl(n: Int) = FlowLabel(n.toString)

    val edges = Seq(
      1 -> 100,
      100 -> 2,
      100 -> 5,
      2 -> 3,
      3 -> 3,
      3 -> 4,
      4 -> 13,
      5 -> 6,
      5 -> 7,
      6 -> 4,
      6 -> 8,
      7 -> 8,
      7 -> 12,
      8 -> 5,
      8 -> 13,
      1 -> 9,
      9 -> 10,
      9 -> 11,
      10 -> 12,
      11 -> 12,
      12 -> 13
    )
    def blockFor(l: FlowLabel) = {
      val outgoing = edges.filter(_._1 == l.name.toInt).map(e => lbl(e._2))
      val jump = outgoing match {
        case Seq(a, b) => FlowGotoCond(FlowVar("C", 0), a, b)
        case Seq(a) => FlowGoto(a)
        case Seq() => FlowReturn(FlowVar("R", 0))
        case _ => fail(l + " had too many outgoind edges " + outgoing)
      }
      FlowBlock(Seq.empty, Seq.empty, jump)
    }
    val allLabels = edges.flatMap { case (x, y) => Seq(lbl(x), lbl(y)) }
    val flowGraph = new FlowGraph(allLabels.map(l => (l -> blockFor(l))), lbl(1))

    // It's a good idea to draw the graphviz of `flowGraph` and `domTree` if you need to debug this

    val domTree = DominatorTree(flowGraph)

    val expectedEdges = Seq(
      1 -> 9,
      1 -> 12,
      1 -> 13,
      9 -> 10,
      9 -> 11,
      100 -> 2,
      100 -> 4,
      100 -> 5,
      2 -> 3,
      5 -> 7,
      5 -> 8,
      5 -> 6
    )
    for ((from, to) <- expectedEdges) {
      domTree.children(lbl(from)) should contain (lbl(to))
      domTree.parent(lbl(to)) should be (lbl(from))
    }

    assert(domTree.dominates(lbl(1), lbl(1)), "node 1 should dominate itself")
    assert(domTree.dominates(lbl(5), lbl(5)), "node 5 should dominate itself")

    domTree.children(FlowGraph.entry) should be (Set(lbl(1)))
    domTree.parent(lbl(1)) should be (FlowGraph.entry)
    domTree.children(lbl(13)) should be (Set(FlowGraph.exit))
    domTree.parent(FlowGraph.exit) should be (lbl(13))

    val df = domTree.dominanceFrontier
    df(lbl(5)) should be (Set(4, 5, 12, 13).map(lbl))
    df(lbl(2)) should be (Set(4).map(lbl))
    df(lbl(3)) should be (Set(3, 4).map(lbl))
    df(lbl(8)) should be (Set(5, 13).map(lbl))
    df(lbl(3)) should be (Set(3, 4).map(lbl))
    df(lbl(1)) should be (Set.empty)
  }
}
