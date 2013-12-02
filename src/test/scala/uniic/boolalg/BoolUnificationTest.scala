package uniic.boolalg
import org.scalatest.FreeSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._

class BoolUnificationTest
    extends FreeSpec
    with BoolTermDsl
    with Checkers
    with ArbitraryBoolTerms {

  import BoolUnification._
  import StringsAsBVars._

  "In boolean unification" - {
    testUnifies("x", "x")
    testUnifies("x", "y")

    testUnifies("x" /\ "y", "x" /\ "y")

    testUnifies("x" \/ "y", "x" /\ "y")
    testUnifies("x" \/ "y", "x" /\ !"y")
    testUnifies("x" \/ "y", !"x" /\ "y")
    testDoesNotUnify("x" \/ "y", !"x" /\ !"y")

    testUnifies("x" \/ "y", "x" /\ "y" /\ "z")

    testUnifies("x" \/ ("y" \/ "z"), ("z" \/ "x") \/ "y")

    "any term should unify with itself: " in {
      check { t: BoolTerm => unify(t, t, t.variables).isDefined }
    }

    "any term and its negation should not unify: " in {
      check { t: BoolTerm => unify(t, !t, t.variables) == None }
    }

    "constants should not be substituted in unification: " - {
      val vars = Set("x", "y", "z").map(BVar(_))

      testUnifies("x", "b", vars)
      testDoesNotUnify("a", "b", vars)

      testUnifies("x" \/ "y", "a" \/ "b", vars)
      testUnifies("b" \/ "a", "a" \/ "b", vars)
      testUnifies("x" /\ "y", "a" /\ "b", vars)
      testUnifies("b" /\ "a", "a" /\ "b", vars)

      testUnifies("x" \/ "y", "a" /\ "b", vars)
      testDoesNotUnify("a" \/ "b", "a" /\ "b", vars)

      testUnifies("x" <--> "y", "z" <--> "z", vars)
      testDoesNotUnify("a" <--> "b", "z" <--> "z", vars)

      testUnifies("x" <--> "b", "y" <--> "z", vars)
      testUnifies("a" <--> "b", "b" <--> "a", vars)
    }
  }

  def testUnifies(t1: BoolTerm, t2: BoolTerm) {
    (t1 + " should unify with " + t2) in {
      assertUnifies(t1, t2)
    }
  }

  def testUnifies(t1: BoolTerm, t2: BoolTerm, vars: Set[BVar]) {
    (t1 + " should unify with " + t2 + " (vars: " + vars.mkString(", ") + ")") in {
      assertUnifies(t1, t2)
    }
  }

  def testDoesNotUnify(t1: BoolTerm, t2: BoolTerm) {
    (t1 + " should not unify with " + t2) in {
      assertDoesNotUnify(t1, t2)
    }
  }

  def testDoesNotUnify(t1: BoolTerm, t2: BoolTerm, vars: Set[BVar]) {
    (t1 + " should not unify with " + t2 + " (vars: " + vars.mkString(", ") + ")") in {
      assertDoesNotUnify(t1, t2, vars)
    }
  }

  def assertUnifies(t1: BoolTerm, t2: BoolTerm) {
    assertUnifies(t1, t2, t1.variables ++ t2.variables)
  }

  def assertUnifies(t1: BoolTerm, t2: BoolTerm, vars: Set[BVar]) {
    unify(t1, t2, vars) match {
      case Some(u) => {
        val s1 = t1.subst(u)
        val s2 = t2.subst(u)
        val eq = (s1 <--> s2).quickSimplify
        assert(eq.isTautology,
               "invalid unification result: " + u + "\n" +
               "equivalence after subst was " + eq)
      }
      case None => fail("No unifier obtained")
    }
  }

  def assertDoesNotUnify(t1: BoolTerm, t2: BoolTerm) {
    assertDoesNotUnify(t1, t2, t1.variables ++ t2.variables)
  }

  def assertDoesNotUnify(t1: BoolTerm, t2: BoolTerm, vars: Set[BVar]) {
    unify(t1, t2, vars) match {
      case Some(u) => {
        fail("No unifier expected but got S = " + u + ".\n" +
          "S t1 = " + t1.subst(u) + "\n" +
          "S t2 = " + t2.subst(u) + "\n" +
          "simplify(S t1) = " + t1.subst(u).simplify + "\n" +
          "simplify(S t2) = " + t2.subst(u).simplify + "\n")
      }
      case None =>
    }
  }
}