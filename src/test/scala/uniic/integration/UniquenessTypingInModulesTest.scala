package uniic.integration

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.types._

class UniquenessTypingInModulesTest
  extends FreeSpec
  with ShouldMatchers
  with ModuleLevelItBase
{
  import TypeDSL._

  "function taking and returning an unique" in {
    """
    module testcase
    def f(x: IntArray^u): IntArray^u {
      return x;
    }
    def main(): Int {
      a := f(mkIntArray(5));
      return 0;
    }
    """.typeOfMain should beEquivalentTo (
      "forall u:A. () -(u)> Int"
    )
  }
}
