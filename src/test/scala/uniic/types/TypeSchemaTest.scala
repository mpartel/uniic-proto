package uniic.types
import org.scalatest.FreeSpec
import org.scalatest.matchers._
import uniic.misc.TypeError

class TypeSchemaTest extends FreeSpec with ShouldMatchers with TypeDSL with TypeSchemaAssertions {

  val t1 = TVar("t1")
  val u1 = AVar("u1")
  val t2 = TVar("t2")
  val u2 = AVar("u2")
  val t3 = TVar("t3")
  val u3 = AVar("u3")

  def unify(kp: (Rank1Kinded, Rank1Kinded)) = new Matcher[TypeTransformation] {
    def apply(unifier: TypeTransformation) = {
      val (input, expected) = (kp._1, kp._2)
      val actual = unifier.applyKinded(kp._1)
      MatchResult(
        unifier.applyKinded(kp._1) == kp._2,
        "Got unifier " + unifier + " which unified " + input + " to " + actual + " but " + expected + " expected",
        "Got unifier " + unifier + " which unified " + input + " to " + actual + " when it shouldn't have"
      )
    }
  }

  "TypeSchema" - {
    "getInstantiationTo and canBeInstantiatedFrom" - {
      "finds an empty unifier for identical (variableless) type schemas" in {
        def ts = TypeSchema(Seq.empty, TInt(u1))
        val unifier = ts.getInstantiationTo(ts)
        unifier should unify (u1 -> u1)
      }

      "fails with a TypeError if unification is not possible" in {
        evaluating { TypeSchema(Seq.empty, t1(u1)).getInstantiationTo(TypeSchema(Seq.empty, t2(u2))) } should produce [TypeError]
      }

      "produces a unifier that translates `this.ty` into `that.ty`" in {
        val ts1 = TypeSchema(Seq(t1, u1), t1(u1))
        val ts2 = TypeSchema(Seq(t2, u2), t2(u2))
        val unifier = ts1.getInstantiationTo(ts2)
        unifier should unify (t1(u1) -> t2(u2))
        unifier should unify (t2(u2) -> t2(u2))
      }

      "does not produce a unifier that would translate `that.ty` to `this.ty`" in {
        val ts1 = TypeSchema(Seq(), t1(u1))
        val ts2 = TypeSchema(Seq(t2, u2), t2(u2))
        evaluating { ts1.getInstantiationTo(ts2) } should produce [TypeError]
      }

      "produces a unifier for the variable names in `this.vars` even if they clash with `that.vars`" in {
        val ts1 = TypeSchema(Seq(t1, u1), t1(u1))
        val ts2 = TypeSchema(Seq(t1, u1), t2(u2))
        val unifier = ts1.getInstantiationTo(ts2)
        unifier should unify (t1(u1) -> t2(u2))
      }

      "produces a unifier that swaps variables if necessary" in {
        val ts1 = TypeSchema(Seq(u1, u2, u3), mkTTuple(TInt(u1), TInt(u2))(u3))
        val ts2 = TypeSchema(Seq(u1, u2, u3), mkTTuple(TInt(u2), TInt(u1))(u3))
        val unifier = ts1.getInstantiationTo(ts2)
        unifier should unify (t1(u1) -> t1(u2))
        unifier should unify (t1(u2) -> t1(u1))
      }

      "doesn't depend on variable names (disjoint names)" in {
        val ts1 = forall(t1, u1)(t1(u1) :-(ANonUniq)> t1(u1))
        val ts2 = forall(t2, u2)(t2(u2) :-(ANonUniq)> t2(u2))
        ts1 should beInstantiatableFrom (ts2)
      }

      "doesn't depend on variable names (overlapping names)" in {
        {
          val ts1 = forall(TVar("a"), TVar("b"))(TVar("a")(u1) :-(ANonUniq)> TVar("b")(u1))
          val ts2 = forall(TVar("a"), TVar("b"))(TVar("b")(u1) :-(ANonUniq)> TVar("a")(u1))
          ts1 should beInstantiatableFrom (ts2)
        }
      }

      "doesn't depend on variable order" in {
        val ts1 = forall(t1, u1)(t1(u1) :-(ANonUniq)> t1(u1))
        val ts2 = forall(u2, t2)(t2(u2) :-(ANonUniq)> t2(u2))
        ts1 should beInstantiatableFrom (ts2)
      }

      "ignores unused variables" in {
        val ts1 = forall(t1, u1, u2, u3)(t1(u1) :-(ANonUniq)> t1(u1))
        val ts2 = forall(t2, u2, t1, t3)(t2(u2) :-(ANonUniq)> t2(u2))
        ts1 should beInstantiatableFrom (ts2)
      }

      "detects incompatible types" in {
        val ts1 = forall(t1, u1)(t1(u1) :-(ANonUniq)> t1(u1))
        val ts2 = forall(t1, u1)(t1(u1) :-(AUniq)> t1(u1))
        ts1 should not (beInstantiatableFrom (ts2))
      }
    }
  }
}