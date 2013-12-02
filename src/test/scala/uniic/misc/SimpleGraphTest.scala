package uniic.misc

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class SimpleGraphTest extends FreeSpec with ShouldMatchers {
  "findCycle" - {
    "finds self-references" in {
      val in = Map(1 -> 1, 2 -> 3)
      SimpleGraph.fromMap(in).findCycle(1) should be (Some(Seq(1)))
    }

    "finds a cycle of 2 edges" in {
      val in = MultiMap(1 -> 2, 2 -> 1, 1 -> 3, 2 -> 4).toImmutable
      SimpleGraph.fromMultiMap(in).findCycle(1) should be (Some(Seq(1, 2)))
    }

    "finds a cycle of 3 edges" in {
      val in = MultiMap(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 3 -> 1).toImmutable
      SimpleGraph.fromMultiMap(in).findCycle(1) should be (Some(Seq(1, 2, 3)))
    }

    "does not find cycles in a DAG" in {
      val in = MultiMap(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4).toImmutable
      SimpleGraph.fromMultiMap(in).findCycle(1) should be (None)
    }
  }
}
