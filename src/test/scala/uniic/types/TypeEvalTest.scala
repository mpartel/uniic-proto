package uniic.types
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.misc.TypeError
import uniic.test.TypeMatchers
import uniic.stdlib.Stdlib

// There is some overlap with TgrsIntegrationTest
class TypeEvalTest extends FreeSpec with ShouldMatchers with TypeMatchers {
  import TypeEval._

  val defaultSymTab = Stdlib.typeSymTab

  "TypeEval" - {
    "evaluates types" in {
      val result = evalKinded(defaultSymTab, TEFun(TEAttribution(TEVar("Int"), TEUniq), TEAttribution(TEVar("Bool"), TENonUniq)))
      result should be (TFun(TInt(AUniq), TBool(ANonUniq)))
    }

    "leaves undefined variables in as TVars or AVars" - {
      "t^u" in {
        val result = evalKinded(defaultSymTab, TEAttribution(TEVar("t"), TEVar("u")))
        result should be (Type(TVar("t"), AVar("u")))
      }
      "t" in {
        val result = evalKinded(defaultSymTab, TEVar("t"))
        result should be (TVar("t"))
      }
      "t1 -> t2^u" in {
        val result = evalKinded(defaultSymTab, TEFun(TEVar("t1"), TEAttribution(TEVar("t2"), TEVar("u"))))
        result should be (TFun(TVar("t1")(ANonUniq), TVar("t2")(AVar("u"))))
      }
    }

    "kind checks" in {
      evaluating { evalKinded(defaultSymTab, TEAttribution(TEVar("Int"), TEVar("Int"))) } should produce [TypeError]
      evaluating { evalKinded(defaultSymTab, TEAttribution(TEUniq, TEUniq)) } should produce [TypeError]
      evaluating { evalKinded(defaultSymTab, TEFun(TEUniq, TEUniq)) } should produce [TypeError]
    }

    "substitutes type aliases" in {
      val symTab = defaultSymTab ++ Map("Foo" -> TInt, "Bar" -> AUniq)
      val result = evalKinded(symTab, TEAttribution(TEVar("Foo"), TEVar("Bar")))
      result should be (TInt(AUniq))
    }

    "adds implicit non-unique annotations" - {
      val symTab = defaultSymTab ++ Map("Foo" -> TInt)
      "Foo -> Foo*  =>  Int+ -(+)> Int+" in {
        val result = evalType(symTab, TEFun(TEVar("Foo"), TEAttribution(TEVar("Foo"), TEUniq)))
        val expected = TFun(TInt(ANonUniq), TInt(AUniq))(ANonUniq)
        result should be (expected)
      }
      "Foo -> Foo -> Foo  =>  Int+ -(+)> Int+ -(+)> Int+" in {
        val result = evalType(symTab, TEFun(TEVar("Foo"), TEFun(TEVar("Foo"), TEVar("Foo"))))
        val expected = TFun(TInt(ANonUniq), TFun(TInt(ANonUniq), TInt(ANonUniq))(ANonUniq))(ANonUniq)
        result should be (expected)
      }
    }

    "disallows multiple type attributes" in {
      evaluating {
        evalType(defaultSymTab, TEVar("Foo").withAttr(TEUniq).withAttr(TENonUniq))
      } should produce [TypeError]
    }

    "handles type schemas" in {
      val symTab = defaultSymTab ++ Map("t" -> TInt)
      val result = evalKinded(symTab, TESchema(Seq(("t", KBaseType), ("u", KTypeAttr)), TEAttribution(TEVar("t"), TEVar("u"))))
      result should be (TypeSchema(Seq(TVar("t"), AVar("u")), Type(TVar("t"), AVar("u"))))
    }

    "evalTypeSchema() adds implicit schema on bare type when needed" in {
      val result = evalTypeSchema(defaultSymTab, TEVar("Int")(TEUniq))
      result should be (TypeSchema(Seq.empty, Type(TInt, AUniq)))
    }
  }
}
