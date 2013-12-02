package uniic.integration

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.types._

class MutualRecursionOfToplevelFunctions
  extends FreeSpec
  with ShouldMatchers
  with ModuleLevelItBase
{
  import TypeDSL._

  "mutual recursion of toplevel functions" in {
    val code = """
      module testcase
      def f(x: Int): Int {
        if x > 0 then {
          return g(x);
        } else {
          return x;
        }
      }
      def g(x: Int): Int {
        return f(x - 1);
      }
      def main(): Int {
        return f(5);
      }
      """

    val program = new ImperativeProgram(code)
    val typeMap = program.linkedProgram.nonborrowingTypeMap
    typeMap("main") should beEquivalentTo ("forall u:A. () -(u)> Int")
    typeMap("f") should beEquivalentTo ("forall u:A. (Int) -(u)> Int")
    typeMap("g") should beEquivalentTo ("forall u:A. (Int) -(u)> Int")

    program.callingMain should evaluateTo ("0")
  }
}
