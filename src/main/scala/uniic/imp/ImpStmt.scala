package uniic.imp

import uniic.fun._

sealed trait ImpStmt

case class ImpReturn(expr: ImpExpr) extends ImpStmt

case class ImpSet(variable: ImpVar, expr: ImpExpr) extends ImpStmt

case class ImpSplitTuple(vars: Seq[ImpVar], expr: ImpExpr) extends ImpStmt {
  assert(vars.size > 1, "ImpSplitTuple with " + vars.size + " vars")
}

case class ImpIf(condExpr: ImpExpr, thenBody: ImpStmt, elseBody: ImpStmt) extends ImpStmt

case class ImpWhile(condExpr: ImpExpr, body: ImpStmt) extends ImpStmt

/** A block of multiple statements.
  *
  * The parser collapses blocks of one statement into just that statement. */
case class ImpBlock(stmts: Seq[ImpStmt]) extends ImpStmt


sealed trait ImpExpr extends ImpStmt

case class ImpVar(name: String) extends ImpExpr

case class ImpMakeTuple(args: Seq[ImpExpr]) extends ImpExpr

case class ImpCall(expr: ImpExpr, args: Seq[ImpArg]) extends ImpExpr

sealed trait ImpArg
case class ImpNormalArg(expr: ImpExpr) extends ImpArg
case class ImpBorrowedArg(v: ImpVar) extends ImpArg

sealed trait ImpConst extends ImpExpr
case object ImpUnit extends ImpConst
case class ImpBool(v: Boolean) extends ImpConst
case class ImpInt(n: Int) extends ImpConst
