package uniic.test

import uniic.flow._
import uniic.imp._
import uniic.misc._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult
import scala.collection.GenSeq
import uniic.types.ParamMode
import uniic.types.MNone

trait FlowTestUtils {
  implicit def strToLabel(s: String) = FlowLabel(s)

  case class BlockBuilder(
    phiStmts: Seq[FlowPhiStmt],
    basicStmts: Seq[FlowBasicStmt]
  ) {
    def phi(outVar: String, inVars: String*) = {
      val phiStmt = FlowPhiStmt(FlowVar(outVar, 0), inVars.map(v => FlowVar(v, 0)))
      BlockBuilder(phiStmts :+ phiStmt, basicStmts)
    }
    def setConst(v: String) = {
      BlockBuilder(phiStmts, basicStmts :+ FlowSetConst(FlowVar(v, 0), ImpInt(0)))
    }
    def makeTuple(outVar: String, vars: String*) = {
      BlockBuilder(phiStmts, basicStmts :+ FlowMakeTuple(
        FlowVar(outVar, 0), vars.map(v => FlowVar(v, 0))
      ))
    }
    def jump(to: FlowLabel) = {
      FlowBlock(phiStmts, basicStmts, FlowGoto(to))
    }
    def jump(v: String, to1: FlowLabel, to2: FlowLabel) = {
      FlowBlock(phiStmts, basicStmts, FlowGotoCond(FlowVar(v, 0), to1, to2))
    }
    def ret(v: String) = {
      FlowBlock(phiStmts, basicStmts, FlowReturn(FlowVar(v, 0)))
    }
  }

  object BlockBuilder extends BlockBuilder(Seq.empty, Seq.empty)

  def mkFlowGraph(firstBlock: (String, FlowBlock), blocks: (String, FlowBlock)*) = {
    val allBlocks = (firstBlock +: blocks).map { case (name, b) => FlowLabel(name) -> b }
    new FlowGraph(allBlocks, firstBlock._1)
  }

  def mkFlowGraph(stmts: FlowStmt*) = {
    FlowGraph.fromStatements(stmts)
  }

  def mkBlock(stmts: FlowStmt*) = FlowBlock.fromStatements(stmts)

  /** Enables the syntax `'x|3` for `FlowVar("x", 3)` */
  implicit class SymbolToVarDsl(s: Symbol) {
    def |(n: Int) = FlowVar(s.name, n)
    def @@(out: FlowVar) = FlowBorrowedArg(FlowVar(s.name, 0), out)
  }

  /** Enables the syntax `'x` for `FlowVar("x", 0)` */
  implicit def symbolToFlowVar(s: Symbol): FlowVar = FlowVar(s.name, 0)

  /** Enables the syntax `'x` for `FlowNormalArg(FlowVar("x", 0))` */
  implicit def symbolToNormalArg(s: Symbol): FlowNormalArg = FlowNormalArg(FlowVar(s.name, 0))

  implicit def varToNormalArg(v: FlowVar): FlowNormalArg = FlowNormalArg(v)

  /** Enables the syntax `'x|3 @@ 'x|4` for `(FlowVar("x", 3), MBorrowed)` */
  implicit class VarModeDsl(v: FlowVar) {
    def @@(out: FlowVar) = FlowBorrowedArg(v, out)
  }

  /** Defaults symbols to flowvars with mode MNone when passed as function arguments */
  implicit def symbolDefaultMode(s: Symbol): (FlowVar, ParamMode) = (FlowVar(s.name, 0), MNone)

  /** Defaults flowvars to mode MNone when passed as function arguments */
  implicit def symbolDefaultMode(v: FlowVar): (FlowVar, ParamMode) = (v, MNone)


  def equivalentTo(expected: Seq[FlowStmt]): BeMatcher[GenSeq[FlowStmt]] = {
    new BeMatcher[GenSeq[FlowStmt]] {
      def apply(leftGen: GenSeq[FlowStmt]) = {
        val left = leftGen.seq // For some reason, ScalaTest 2.0.M5b insists on GenSeq instead of Seq. ScalaTest 1.9.1 didn't.
        val error: Option[String] = {
          if (left.length != expected.length) {
            Some("the lengths differ")
          } else {
            left.zip(expected).zipWithIndex.find { case ((a, b), i) => a != b }.map {
              case (_, i) => "difference in the " + Inflections.ordinal(i + 1) + " command"
            }
          }
        }

        lazy val diff = ScalaDiffUtils.diffToString(expected.map(_.toString), left.map(_.toString), new ScalaDiffUtils.DiffToStringOptions {
          override val originalFilename = "expected"
          override val revisedFilename = "actual"
        })
        lazy val leftText = left.mkString("\n")
        lazy val expectedText = expected.mkString("\n")
        if (!error.isEmpty) {
          println(diff)
        }

        MatchResult(
          error.isEmpty,
          leftText + "\n was not deeply equivalent to what was expected (" + error.getOrElse("no error") + ")\n\nDiff (also in stdout):\n" + diff,
          leftText + "\n was deeply equivalent to what was expected."
        )
      }
    }
  }
}
