package uniic.misc
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class UnicodeSpec extends FlatSpec with ShouldMatchers {
  "Unicode.subscript" should "convert integer to unicode subscript string" in {
    Unicode.subscript(0) should be ("₀")
    Unicode.subscript(1) should be ("₁")
    Unicode.subscript(1234567890) should be ("₁₂₃₄₅₆₇₈₉₀")
    Unicode.subscript(-1234567890) should be ("₋₁₂₃₄₅₆₇₈₉₀")
  }
}