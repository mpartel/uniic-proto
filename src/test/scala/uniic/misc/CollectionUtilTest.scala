package uniic.misc

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class CollectionUtilTest extends FreeSpec with ShouldMatchers {
  "uniq" in {
    CollectionUtil.uniq(Seq(1,7,1,3,3,7,1,2)) should be (Seq(1,7,3,2))
  }
}
