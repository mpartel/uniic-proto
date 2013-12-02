package uniic.boolalg

import org.scalatest.FreeSpec
import org.scalatest.matchers._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class BoolTermSpec extends FreeSpec with ShouldMatchers with Checkers with ArbitraryBoolTerms with BoolTermDsl {

  import StringsAsBVars._

  "BoolTerm" - {
    "variable substitution should substitute all instances of a given variable" in {
      val t1 = ("x" /\ ("y" \/ !"x"))
      val t2 = t1.subst(Seq(BVar("x") -> BVar("z"), BVar("y") -> BVar("w")))
      t2 should be ("z" /\ ("w" \/ !"z"))
    }

    "tautology" in {
      BTrue should be a ('tautology)
      BFalse should not be a ('tautology)
      (BTrue \/ "x") should be a ('tautology)
      (BFalse \/ "x") should not be a ('tautology)

      ("x" \/ !"x") should be a ('tautology)
      (("x" /\ "y") \/ "y" \/ !"y") should be a ('tautology)
      (("x" \/ "y") /\ "y") should not be a ('tautology)
    }

    "quick simplification" in {
      check { t: BoolTerm =>
        collect(t.size) {
          val simplified = t.quickSimplify
          lazy val label = (t + " <-- " + simplified + "\n")
          (simplified --> t).isTautology :| label
        }
      }
    }

    "should know its variables" in {
      ("x" \/ "y" /\ "z" /\ !"w").variables should equal (Set[BVar]("x", "y", "z", "w"))
      ("x" \/ "x" /\ "x" /\ !"x").variables should equal (Set[BVar]("x"))
    }

    "can be evaluated with a given symbol table" in {
      (("x" \/ "y") /\ !"z").eval(_ match {
        case BVar("x") => true
        case BVar("y") => false
        case BVar("z") => false
      }) should be (true)

      (("x" \/ "y") /\ !"z").eval(_ match {
        case BVar("x") => true
        case BVar("y") => false
        case BVar("z") => true
      }) should be (false)
    }
  }

}