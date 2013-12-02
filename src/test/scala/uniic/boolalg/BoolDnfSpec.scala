package uniic.boolalg
import org.scalatest.prop.Checkers
import org.scalatest.FreeSpec
import org.scalacheck.Prop

class BoolDnfSpec extends FreeSpec with BoolTermDsl with Checkers with ArbitraryBoolTerms {
  import Prop._
  import StringsAsBVars._

  "Boolean terms in DNF form" - {
    "should be equivalent to the originals" in {
      check({b: BoolTerm =>
        val maybeDnf = BoolDnf(b)
        maybeDnf match {
          case Some(dnf) =>
            (dnf + " <=> " + b) |: (dnf.toBoolTerm <--> b).isTautology
          case None =>
            ("Tautology because no DNF: " + b) |: b.isTautology
        }
      })
    }

    "No DNF from for trivially true terms" in {
      check({b: BoolTerm => BoolDnf(b \/ BTrue) == None})
    }
  }
}