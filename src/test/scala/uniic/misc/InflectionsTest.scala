package uniic.misc
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class InflectionsTest extends FreeSpec with ShouldMatchers {
  import Inflections._

  "ordinal(n)" in {
    ordinal(1) should be ("1st")
    ordinal(2) should be ("2nd")
    ordinal(3) should be ("3rd")
    ordinal(4) should be ("4th")
    ordinal(5) should be ("5th")
    ordinal(9) should be ("9th")

    ordinal(10) should be ("10th")
    ordinal(11) should be ("11th")
    ordinal(12) should be ("12th")
    ordinal(13) should be ("13th")
    ordinal(14) should be ("14th")

    ordinal(20) should be ("20th")
    ordinal(21) should be ("21st")
    ordinal(22) should be ("22nd")
    ordinal(23) should be ("23rd")
    ordinal(24) should be ("24th")

    ordinal(1010) should be ("1010th")
    ordinal(1011) should be ("1011th")
    ordinal(1012) should be ("1012th")
    ordinal(1013) should be ("1013th")
    ordinal(1014) should be ("1014th")

    ordinal(1020) should be ("1020th")
    ordinal(1021) should be ("1021st")
    ordinal(1022) should be ("1022nd")
    ordinal(1023) should be ("1023rd")
    ordinal(1024) should be ("1024th")
  }
}