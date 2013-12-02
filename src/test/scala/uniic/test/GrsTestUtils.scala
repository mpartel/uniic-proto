package uniic.test

import uniic.grs._
import org.scalatest.Suite
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait GrsTestUtils extends GrsDSL {
  this: Suite =>

  def beEquivalentTo[K](rightSpecs: GrsSpec[K]*): Matcher[Grs] = {
    beEquivalentTo(mkGrs(rightSpecs: _*))
  }

  def beEquivalentTo(right: Grs): Matcher[Grs] = Matcher { left: Grs =>
    val leftToRight = left.isEquvalentTo(right)
    val rightToLeft = right.isEquvalentTo(left)
    if (leftToRight != rightToLeft) {
      fail(s"Equivalence of $left and $right was inconsistent ($leftToRight, $rightToLeft)")
    }
    MatchResult(
      leftToRight && rightToLeft,
      left + " was not equivalent to " + right,
      left + " was equivalent to " + right
    )
  }
}
