package uniic.test

import uniic.imp._
import uniic.misc._
import uniic.flow._
import uniic.fun._
import uniic.types._
import uniic.stdlib.Stdlib
import uniic.parsers.LangParsers

trait ImpTestUtils {
  def toFlowGraph(code: String): FlowGraph = {
    FlowGraph.fromStatements(toFlow(code))
  }

  def toSsaFlowGraph(code: String): FlowGraph = {
    FlowGraph.fromStatements(toSsa(code))
  }

  def toSsa(code: String): Seq[FlowStmt] = {
    val statements = toFlow(code)
    val flowGraph = FlowGraph.fromStatements(statements)
    val ssaFlowGraph = FlowToSSA.apply(flowGraph, Set.empty)
    val labelsInOrder = statements.flatMap {
      case lbl: FlowLabel => Some(lbl)
      case _ => None
    }
    val ssaStatements = labelsInOrder.flatMap { lbl =>
      lbl +: ssaFlowGraph.blocks(lbl).allStmts
    }
    ssaStatements
  }

  def toFlow(code: String): Seq[FlowStmt] = {
    ImpToFlow.apply(parseImp(code))
  }

  def parseImp(code: String): ImpStmt = {
    parseImp(code, Stdlib.defaultOperatorSet)
  }

  def parseImp(code: String, operators: LangParsers.OperatorSet): ImpStmt = {
    LangParsers.parseImpStmt(code, operators) match {
      case Right(expr) => expr
      case Left(error) => throw new CompilerException(error)
    }
  }
}