package uniic.grs

import org.scalatest.FunSuite
import uniic.grs._
import uniic.test.GrsTestUtils
import uniic.parsers.LangParsers
import uniic.stdlib.Stdlib
import uniic.fun.FunToGrs
import uniic.fun.analysis.FunTree
import org.scalatest.BeforeAndAfterEach

class GrsEvalTest extends FunSuite with BeforeAndAfterEach with GrsTestUtils {
  var debug = false

  override def afterEach() {
    debug = false
  }

  def t(fromTo: (String, String), wantDebug: Boolean = false) = {
    fromTo match {
      case (exprStr, expectedStr) => {
        test(exprStr + " => " + expectedStr) {
          debug = wantDebug
          check(exprStr, expectedStr)
        }
      }
    }
  }

  def check(exprStr: String, expectedStr: String) = {
    val exprFun = LangParsers.parseFunExprOrThrow(exprStr, Stdlib.defaultOperatorSet)
    val expectedFun = LangParsers.parseFunExprOrThrow(expectedStr, Stdlib.defaultOperatorSet)
    val exprGrs = FunToGrs(new FunTree(exprFun), Stdlib.symTabWithoutTypes)
    val expectedGrs = FunToGrs(new FunTree(expectedFun), Stdlib.symTabWithoutTypes)
    val evaluatedGrs = GrsEval.eval(exprGrs, debug)
    assert(
      evaluatedGrs.isEquvalentTo(expectedGrs),
      evaluatedGrs + " did not match " + expectedGrs
    )
  }

  t("let x = 3 in x" -> "3")
  t("let x = 3 in (x, x)" -> "(3, 3)")
  t("let x = y and y = 3 in x" -> "3")

  t("let f = \\y. y in f(4)" -> "4")
  t("let f = \\y. f in 5" -> "5")
  t("let f = \\y. x and x = 10 in f(3)" -> "10")
  t("let f = \\y. y + 1 and g = \\y. y * 2 in f(g(4))" -> "9")

  t("1 + 2 * 3" -> "7")

  t("if 1 + 1 == 2 then 3 else 4" -> "3")
  t("if 1 + 1 == 20 then 3 else 4" -> "4")

  t("(1, (2, 3)) match { case (1, 2) => (10, 10); case (1, y) => y }" -> "(2, 3)")
  t("(1, (2, 3)) match { case (1, 2) => (10, 10); case (x, y) => y }" -> "(2, 3)")

  test("factorial") {
    val code =
      """
      let fact = \n.
        if n <= 1
          then 1
          else n * fact(n - 1)
      in fact(5)
      """
    check(code, "120")
  }

  test("nested closures") {
    val code =
      """
      let f = \x.
        let g = \y. x
        in g
      in (f(1)(5), f(2)(10))
      """
    check(code, "(1, 2)")
  }

  test("mutual recursion") {
    val code =
      """
      let f = \x. if x > 5 then g(x - 1) else 123
      and g = \y. f(y)
      in (f(7))
      """
    check(code, "123")
  }
}
