package uniic.types
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.misc.TypeError

class TypeUtilsTest extends FreeSpec with ShouldMatchers with TypeDSL {
  import TypeUtils._

  "findVars method" - {
    // We don't use parseType here since it calls findVars

    "finds all TVars and AVars in a type" in {
      val ty = mkTTuple(TVar("t")(ANonUniq), TVar("t")(AVar("u")))(ANonUniq)
      val result = findVars(ty)
      result should be (Set(TVar("t"), AVar("u")))
    }

    "throws if there is a TVar and an AVar with the same name" in {
      evaluating { findVars(mkTTuple(TVar("t")(ANonUniq), TVar("v")(AVar("v")))(ANonUniq)) } should produce [TypeError]
      evaluating { findVars(mkTTuple(TVar("t")(AVar("v")), TVar("v")(ANonUniq))(ANonUniq)) } should produce [TypeError]
    }
  }
}