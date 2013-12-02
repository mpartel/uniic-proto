package uniic.integration

import org.scalatest.Suite
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers._
import uniic.test._
import uniic.misc._
import uniic.types._
import uniic.grs._
import uniic.fun._
import uniic.fun.analysis.FunTree
import uniic.parsers.LangParsers
import uniic.stdlib.Stdlib

trait IntegrationTestBase
  extends Suite
  with BeforeAndAfterEach
  with ExceptionContexts
{
  var symTab = Stdlib.symTab
  var typeSymTab = Stdlib.typeSymTab
  var operatorSet = Stdlib.defaultOperatorSet
  var debug = false
  var debugPrefix = "integration"

  override def afterEach() {
    symTab = Stdlib.symTab
    typeSymTab = Stdlib.typeSymTab
    operatorSet = Stdlib.defaultOperatorSet
    debug = false
    debugPrefix = "integration"
  }

  def debugPrintln(msg: String = "") {
    if (debug) {
      println(msg)
    }
  }

  implicit def parseFunExpr(s: String) = LangParsers.parseFunExprOrThrow(s, operatorSet)
  implicit def parseTypeSchema(s: String) = TypeEval.evalTypeSchema(typeSymTab, LangParsers.parseTypeExprOrThrow(s))

  def evaluateTo(expected: Grs): Matcher[Grs] = {
    Matcher[Grs] { (grs: Grs) =>
      val evaluated = GrsEval.eval(grs, debug)
      MatchResult(
        evaluated.isEquvalentTo(expected),
        s"Program evaluated to $evaluated but expected $expected",
        s"Program unexpectedly evaluated to $evaluated"
      )
    }
  }

  def evaluateTo(code: String): Matcher[Grs] = {
    val tree = new FunTree(parseFunExpr(code))
    evaluateTo(FunToGrs(tree, Stdlib.symTabWithoutTypes))
  }
}