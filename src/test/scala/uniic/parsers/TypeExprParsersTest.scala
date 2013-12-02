package uniic.parsers
import uniic.types._
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.parsing.input.Reader

class TypeExprParsersTest extends FreeSpec with ShouldMatchers with ParsersTestBase with TypeExprDSL {
  override val parsers = new TypeExprParsers {}
  override def mkScanner(reader: Reader[Char]) = new parsers.lexical.Scanner(reader)
  import parsers._

  def p(s: String): ParseResult[TypeExpr] = s parsedWith typeExpr

  "TypeExprParsers" - {
    "primitive types" in {
      p("()") should parseAs (TEUnit)
      p("()*") should parseAs (TEUnit(TEUniq))
      p("()^*") should parseAs (TEUnit(TEUniq))

      p("Bool") should parseAs (tbool)
      p("Bool*") should parseAs (tbool(TEUniq))
      p("Bool^*") should parseAs (tbool(TEUniq))
      p("Bool^u") should parseAs (tbool(TEVar("u")))

      p("Int") should parseAs (tint)
      p("Int*") should parseAs (tint(TEUniq))
      p("Int^*") should parseAs (tint(TEUniq))
      p("Int+") should parseAs (tint(TENonUniq))
      p("Int^+") should parseAs (tint(TENonUniq))
      p("Int^u") should parseAs (tint(TEVar("u")))

      p("(Int)") should parseAs (tint)
      p("(Int)^u") should parseAs (tint(TEVar("u")))
      p("(Int)^(u)") should parseAs (tint(TEVar("u")))
    }

    "uniqueness attributes" in {
      p("Int^u\\/~v") should parseAs (tint("u" \/ !"v"))
      p("Int^~u/\\v") should parseAs (tint(!"u" /\ "v"))
      p("Int^~(u/\\v)") should parseAs (tint(!("u" /\ "v")))
    }

    "tuples" in {
      p("(Int*, Bool+)") should parseAs (mkTETuple(tint(TEUniq), tbool(TENonUniq)))
      p("(Int*, Bool+)+") should parseAs (mkTETuple(tint(TEUniq), tbool(TENonUniq))(TENonUniq))
      p("(Int*, Bool+)*") should parseAs (mkTETuple(tint(TEUniq), tbool(TENonUniq))(TEUniq))
    }

    "function types" in {
      p("Int* -> Int*") should parseAs (tint(TEUniq) :-> tint(TEUniq))
      p("Int* -*> Int*") should parseAs  (tint(TEUniq) :-(TEUniq)> tint(TEUniq))
      p("Int* -u> Int*") should parseAs  (tint(TEUniq) :-("u")> tint(TEUniq))
      p("Int* -(u)> Int*") should parseAs  (tint(TEUniq) :-("u")> tint(TEUniq))
      p("Int^u -+> Bool^v -u> Int*") should parseAs (
        tint("u") :-(TENonUniq)> (tbool("v") :-("u")> tint(TEUniq))
      )
      p("() -> Int*") should parseAs (TEFun(Seq(), TENonUniq, tint(TEUniq)))
      p("""Int* -(u|*\/v)> Int^v""") should parseAs (TEFun(Seq(TEFunParam(tint(TEUniq), MNone)), TEOr(TEUniq, "v"), tint("v")).withAttr("u"))
    }

    "function types with multiple params" in {
      p("(Int*, Int) -> Int*") should parseAs (
        TEFun(Seq(TEFunParam(tint(TEUniq), MNone), TEFunParam(tint, MNone)), tint(TEUniq))
      )
      p("(Int*, Int) -> (Int, Int*)") should parseAs (
        TEFun(Seq(TEFunParam(tint(TEUniq), MNone), TEFunParam(tint, MNone)), TETuple(Seq(tint, tint(TEUniq))))
      )
      p("(Int*, Int) -(u)> (Int, Int*)") should parseAs (
        TEFun(Seq(TEFunParam(tint(TEUniq), MNone), TEFunParam(tint, MNone)), TETuple(Seq(tint, tint(TEUniq)))).withAttr("u")
      )
    }

    "function types with borrowed params" in {
      p("@Int* -> Int*") should parseAs (TEFunParam(tint(TEUniq), MBorrowed) :-> tint(TEUniq))
      p("(@Int*, @Int*) -> Int*") should parseAs (
        TEFun(Seq(TEFunParam(tint(TEUniq), MBorrowed), TEFunParam(tint(TEUniq), MBorrowed)), tint(TEUniq))
      )
    }

    "higher order function types" in {
      p("(Int -> Int) -> Bool") should parseAs ((tint :-> tint) :-> tbool)
      p("(Int+ -+> Int+) -+> Bool+") should parseAs ((tint(TENonUniq) :-(TENonUniq)> tint(TENonUniq)) :-(TENonUniq)> tbool(TENonUniq))
    }

    "extra parens" in {
      p("(Int* -> Int*)") should parseAs (tint(TEUniq) :-> tint(TEUniq))
      p("((Int* -+> Int*))") should parseAs (tint(TEUniq) :-(TENonUniq)> tint(TEUniq))
      p("(((Int* -> Int*)))") should parseAs (tint(TEUniq) :-> tint(TEUniq))
      p("(((Int*, Bool+)+))") should parseAs (mkTETuple(tint(TEUniq), tbool(TENonUniq))(TENonUniq))
    }

    "functions with tuples" in {
      p("((Int*, Bool*)*) -> (Bool, Int)*") should parseAs (
        mkTETuple(tint(TEUniq), tbool(TEUniq))(TEUniq) :-> mkTETuple(tbool, tint)(TEUniq)
      )
    }

    "type schemas" in {
      p("forall t:T. t") should parseAs (
        TESchema(Seq(("t", KBaseType)), TEVar("t"))
      )
      p("forall t:T u:A. t^u -+> Int^u") should parseAs (
        TESchema(Seq(("t", KBaseType), ("u", KTypeAttr)), TEVar("t")("u") :-(TENonUniq)> tint("u"))
      )
      p("forall t:T a:A b:A. (t^a, t^b)") should parseAs (
        TESchema(
          Seq(("t", KBaseType), ("a", KTypeAttr), ("b", KTypeAttr)),
          TETuple(Seq(TEVar("t").withAttr(TEVar("a")), TEVar("t").withAttr(TEVar("b"))))
        )
      )
      p("forall u1:A u2:A u3:A. (Int^u1, Int^u2)^u3") should parseAs (
        TESchema(
          Seq(("u1", KTypeAttr), ("u2", KTypeAttr), ("u3", KTypeAttr)),
          TETuple(Seq(tint("u1"), tint("u2"))).withAttr("u3")
        )
      )
      p("forall u1:A u2:A. (Int -u1> Int, Bool)^u2") should parseAs (
        TESchema(
          Seq(("u1", KTypeAttr), ("u2", KTypeAttr)),
          TETuple(Seq(tint :-("u1")> tint, tbool)).withAttr("u2")
        )
      )
      p("""forall u:A. ((Int, Int)^(u \/ *)) -> Int""") should parseAs (
        TESchema(
          Seq(("u", KTypeAttr)),
          TETuple(Seq(tint, tint)).withAttr(TEOr("u", TEUniq)) :-> tint
        )
      )
    }
  }
}
