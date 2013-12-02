package uniic.integration

import org.scalatest.Suite
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers._
import uniic.test._
import uniic.misc._
import uniic.fun._
import uniic.fun.analysis._
import uniic.grs._
import uniic.imp._
import uniic.flow._
import uniic.types._
import uniic.stdlib._
import uniic.parsers._
import uniic.module._

trait SingleFunctionItBase extends IntegrationTestBase {

  def toGrs(code: String): Grs = {
    val funExpr = parseFunExpr(code)
    val funTree = new FunTree(funExpr)
    FunToGrs(funTree, Stdlib.symTabWithoutTypes)
  }

  implicit class ImperativeBlock(code: String) {
    val impStmt = LangParsers.parseImpStmtOrThrow(code, operatorSet)
    val debugSettings = Stage.DebugSettings.choose(debug)
    val ctx = Stage.Context(Stdlib.symTab, Stdlib.typeSymTab, debugSettings)

    lazy val impToplevel = ImpFunDef(Seq.empty, None, impStmt)
    lazy val impStage = ImpStage(ctx, impToplevel)
    lazy val nonborrowingSSAStage = impStage.to[NonborrowingTypedSSAStage]
    lazy val grsStage = nonborrowingSSAStage.to[TypedGrsStage]

    lazy val grsBodyNode = belowToplevelLambda(grsStage.grs)

    lazy val ssaTy = nonborrowingSSAStage.nonborrowingSsaTypingResult.returnType
    lazy val grsTy = grsStage.typing(grsBodyNode).generalize(Set.empty)

    def ~>(expectedValue: String, expectedType: String) {
      ~>(expectedValue)
      |-(expectedType)
    }
    def ~>(expectedValueStr: String) {
      val evaluated = GrsEval.eval(Grs(grsBodyNode), debug)
      val expected = toGrs(expectedValueStr)
      assert(
        evaluated.isEquvalentTo(expected),
        s"Program evaluated to $evaluated but expected $expected"
      )
    }
    def |-(expectedTypeStr: String) {
      val expected = parseTypeSchema(expectedTypeStr)
      assert(grsTy.isEquivalentTo(ssaTy.generalize(Set.empty)), "GRS type [" + grsTy + "] did not match SSA type [" + ssaTy + "]")
      assert(grsTy.isEquivalentTo(expected), "GRS type [" + grsTy + "] did not match expected type [" + expected + "]")
    }

    def ssaTypeError: Option[TypeError] = {
      catchError[TypeError] {
        ssaTy
      }
    }

    private def catchError[E : Manifest](block: => Unit): Option[E] = {
      var caught = Option.empty[E]
      try {
        block
      } catch {
        case e: E => caught = Some(e)
      }
      caught
    }

    private def belowToplevelLambda(grs: Grs): GrsNode = {
      assert(grs.rootNode.soleChild.value.isInstanceOf[GrsLambda])
      grs.rootNode.soleChild.children(2)
    }

    private def getResultType(grsStage: TypedGrsStage): TypeSchema = {
      require(grsStage.grs.rootNode.soleChild.value.isInstanceOf[GrsLambda])
      val bodyNode = grsStage.grs.rootNode.soleChild.children(2)
      grsStage.typing(bodyNode).generalize(Set.empty)
    }
  }

  val ssaTypecheck = Matcher { left: String =>
    val program = ImperativeBlock(left)
    MatchResult(
      program.ssaTypeError.isEmpty,
      "Program failed to typecheck: " + program.ssaTypeError.getOrElse(""),
      "Faulty program typechecked (SSA typing)"
    )
  }

}
