package uniic.boolalg
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers
import org.scalatest.FreeSpec
import uniic.misc.Unicode

class BoolMinimizerSpec extends FreeSpec with ShouldMatchers with Checkers with BoolTermDsl with ArbitraryBoolTerms {
  import org.scalacheck.Prop._
  import StringsAsBVars._
  import BoolDnf.DontCare

  "Boolean minimizer" - {
    "should produce equivalent terms" in {
      check({bt: BoolTerm =>
        val minimized = BoolMinimizer(bt)
        (bt.quickSimplify + " ==> " + minimized) |: (minimized <--> bt).isTautology
      }, maxSize(60), minSuccessful(500)) // run smaller tests but more of them, since the algo is quite slow for some inputs
    }

    val a = "a": BoolTerm
    val b = "b": BoolTerm
    val c = "c": BoolTerm

    testCase(a \/ (a /\ b)          -> a)
    testCase(a \/ b \/ (a /\ !b)    -> a \/ b)
    testCase((a /\ !b) \/ (a /\ !c) -> (a /\ !b) \/ (a /\ !c))

    testCase(
      List(
        "0000",
        "0001",
        "0010",
        "0011",
        "0101",
        "0111",
        "1000",
        "1010",
        "1100",
        "1101",
        "1111"
      ),
      List(
        "X0X0",
        "X1X1",
        "00XX",
        "1X00"
      )
    )
  }

  def testCase(p: (BoolTerm, BoolTerm)) = {
    val (original, expected) = p
    (original + " should become " + expected) in {
      BoolDnf(original) match {
        case Some(dnf) => BoolMinimizer(dnf) should equal (BoolDnf(expected).get)
        case None => fail("Weird test case")
      }
    }
  }

  def testCase(originalClauses: List[String], expectedClauses: List[String]) = {
    "test case reducing " + originalClauses.length + " clauses to " + expectedClauses.length + " clauses" in {
      val vars = Stream.from(0).map("x" + Unicode.subscript(_)).take(originalClauses(0).length).toArray
      val original = new BoolDnf(vars, originalClauses.map(parseClause(_)).toArray)
      val expected = new BoolDnf(vars, expectedClauses.map(parseClause(_)).toArray)
      BoolMinimizer(original) should equal (expected)
    }
  }

  def parseClause(s: String): Array[Byte] = {
    var result = new Array[Byte](s.length)
    for (i <- 0 until s.length) {
      var ch = s.charAt(i)
      ch match {
        case '0' => result(i) = 0
        case '1' => result(i) = 1
        case 'X' => result(i) = DontCare
        case _ => fail("Weird test case")
      }
    }
    result
  }
}