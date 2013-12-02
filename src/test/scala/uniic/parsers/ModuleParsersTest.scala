package uniic.parsers

import uniic.imp._
import uniic.fun._
import uniic.types._
import uniic.module._
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.stdlib.Stdlib
import scala.util.parsing.input.Reader

class ModuleParsersTest extends FreeSpec with ShouldMatchers with ParsersTestBase {
  object ParsersImpl extends ModuleParsers {
    override val moduleInfixOperators = Stdlib.defaultOperatorSet
  }

  override val parsers = ParsersImpl // ParsersImpl exists to work around a Scala bug ("not found: type $anon")
  override def mkScanner(reader: Reader[Char]) = new parsers.lexical.Scanner(reader)
  import parsers._

  def p(s: String): ParseResult[UncompiledModule] = s parsedWith module

  "parsing a module" in {
    val code =
      """
      module foo

      def f(x: @Int): Bool { return true; }

      let g: (t -> t) = \x. x
      """

    val parsed = p(code).get

    parsed.name should be ("foo")
    parsed.entries should be (
      Seq(
        (
          "f",
          UncompiledModImpEntry(
            ImpFunDef(Seq("x" -> TEFunParam(TEVar("Int"), MBorrowed)), Some(TEVar("Bool")), ImpReturn(ImpBool(true)))
          ),
          TEFun(TEFunParam(TEVar("Int"), MBorrowed), TEVar("Bool"))
        ),
        (
          "g",
          UncompiledModFunEntry(
            FunLambda(Seq("x" -> MissingTypeAnnotation), FunVar("x"))
          ),
          TEFun(TEVar("t"), TEVar("t"))
        )
      )
    )
  }
}
