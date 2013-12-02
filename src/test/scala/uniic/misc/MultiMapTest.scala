package uniic.misc

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class MultiMapTest extends FreeSpec with ShouldMatchers {
  "reverseImmutable" in {
    val in = Map(
      1 -> Set(2, 3),
      2 -> Set(3, 4),
      3 -> Set.empty[Int]
    )
    val expected = Map(
      2 -> Set(1),
      3 -> Set(1, 2),
      4 -> Set(2)
    )

    MultiMap.reverseImmutable(in) should be (expected)
  }
}
