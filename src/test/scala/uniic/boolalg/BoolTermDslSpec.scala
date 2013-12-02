package uniic.boolalg
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class BoolTermDslSpec extends WordSpec with ShouldMatchers with BoolTermDsl {
  "The boolean term DSL" should {

    val x = BVar("x")
    val y = BVar("y")
    val z = BVar("z")

    "allow writing 'not' as '!'" in {
      !(BTrue) should equal (BNot(BTrue))
    }

    "allow writing 'and' and 'or' as '/\' and '\\/'" in {
      (BTrue /\ BFalse) \/ BVar("x") should equal (BOr(BAnd(BTrue, BFalse), BVar("x")))
    }

    "associate 'or' left" in {
      (x \/ y \/ z) should equal (BOr(BOr(x, y), z))
    }

    "associate 'and' left" in {
      (x /\ y /\ z) should equal (BAnd(BAnd(x, y), z))
    }

    "give 'or' higher precedence than 'and'" in {
      (x /\ y \/ z) should equal (BAnd(x, BOr(y, z)))
      (x \/ y /\ z) should equal (BAnd(BOr(x, y), z))
    }

    "allow writing BVar(\"x\") as \"x\" if StringsAsBVars is imported" in {
      import StringsAsBVars._
      ("x" \/ "y") /\ "z" should equal (BAnd(BOr(BVar("x"), BVar("y")), BVar("z")))
    }

    "allow writing an implication using '-->'" in {
      (x --> y) should equal (y \/ !x)
    }

    "allow writing equivalence using '<-->'" in {
      (x <--> y) should equal ((x /\ y) \/ (!x /\ !y))
    }
  }
}