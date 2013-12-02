package uniic.flow

import uniic.fun.FunExpr
import uniic.imp.ImpConst
import uniic.misc.Unicode
import uniic.types.{ParamMode, MNone, MBorrowed}

/** A flowgraph statement. */
sealed trait FlowStmt {
  def resultVars: Seq[FlowVar] = Seq.empty
  def readVars: Seq[FlowVar]
  def mapReadVars(f: PartialFunction[FlowVar, FlowVar]): FlowStmt = {
    mapReadVars(f.orElse(PartialFunction(identity[FlowVar])): FlowVar => FlowVar)
  }
  def mapReadVars(f: FlowVar => FlowVar): FlowStmt
}

case class FlowLabel(name: String) extends FlowStmt with Ordered[FlowLabel] {
  def compare(that: FlowLabel) = this.name.compare(that.name)
  def readVars = Seq.empty
  def mapReadVars(f: FlowVar => FlowVar) = this
  override def toString = "&" + name
}

case class FlowPhiStmt(variable: FlowVar, inputs: Seq[FlowVar]) extends FlowStmt {
  override def resultVars = Seq(variable)
  def readVars = inputs
  def mapReadVars(f: FlowVar => FlowVar) = FlowPhiStmt(variable, inputs.map(f))
  override def toString = variable.toString.padTo(4, ' ') + " := " + Unicode.phiSymbol + inputs.mkString("(", ",", ")")
}

sealed trait FlowBasicStmt extends FlowStmt

case class FlowSetConst(variable: FlowVar, value: ImpConst) extends FlowBasicStmt {
  override def resultVars = Seq(variable)
  def readVars = Seq.empty
  def mapReadVars(f: FlowVar => FlowVar) = this
  override def toString = variable.toString.padTo(4, ' ') + " := " + value
}

case class FlowSet(variable: FlowVar, from: FlowVar) extends FlowBasicStmt {
  override def resultVars = Seq(variable)
  def readVars = Seq(from)
  def mapReadVars(f: FlowVar => FlowVar) = FlowSet(variable, f(from))
  override def toString = variable.toString.padTo(4, ' ') + " := " + from
}

/** Makes `variable` be a version of `from` without borrowing.
  *
  * TODO: might be cleaner to get rid of this and tag a FlowGraph as either using borrowing or not. */
case class FlowBorrowingRemoval(variable: FlowVar, from: FlowVar) extends FlowBasicStmt {
  override def resultVars = Seq(variable)
  def readVars = Seq(from)
  def mapReadVars(f: FlowVar => FlowVar) = FlowBorrowingRemoval(variable, f(from))
  override def toString = variable.toString.padTo(4, ' ') + " := removeBorrowing " + from
}

case class FlowCall(variable: FlowVar, function: FlowVar, args: Seq[FlowArg]) extends FlowBasicStmt {
  override def resultVars = variable +: args.flatMap(_.maybeOut)
  def readVars = function +: args.map(_.in)
  def mapReadVars(f: FlowVar => FlowVar) = FlowCall(variable, f(function), args.map(a => a.mapReadVars(f)))
  override def toString = variable.toString.padTo(4, ' ') + " := " + function + args.mkString("(", ", ", ")")
}

sealed trait FlowArg {
  def in: FlowVar
  def maybeOut: Option[FlowVar]
  def isBorrowing = mode.isBorrowing
  def mode: ParamMode
  def mapReadVars(f: FlowVar => FlowVar): FlowArg
}
case class FlowNormalArg(in: FlowVar) extends FlowArg {
  def maybeOut = None
  def mode = MNone
  def mapReadVars(f: FlowVar => FlowVar) = FlowNormalArg(f(in))
  override def toString = in.toString
}
case class FlowBorrowedArg(in: FlowVar, out: FlowVar) extends FlowArg {
  def maybeOut = Some(out)
  def mode = MBorrowed
  def mapReadVars(f: FlowVar => FlowVar) = FlowBorrowedArg(f(in), out)
  override def toString = in + "@" + out
}

case class FlowMakeTuple(variable: FlowVar, args: Seq[FlowVar]) extends FlowBasicStmt {
  override def resultVars = Seq(variable)
  def readVars = args
  def mapReadVars(f: FlowVar => FlowVar) = FlowMakeTuple(variable, args.map(f))
  override def toString = variable.toString.padTo(4, ' ') + " := " + args.mkString("(", ", ", ")")
}

case class FlowSplitTuple(variables: Seq[FlowVar], tupleVar: FlowVar) extends FlowBasicStmt {
  override def resultVars = variables
  def readVars = Seq(tupleVar)
  def mapReadVars(f: FlowVar => FlowVar) = FlowSplitTuple(variables, f(tupleVar))
  override def toString = variables.mkString("(", ",", ")").padTo(4, ' ') + " := " + tupleVar
}

sealed trait FlowJumpStmt extends FlowStmt

case class FlowGoto(lbl: FlowLabel) extends FlowJumpStmt {
  def readVars = Seq.empty
  def mapReadVars(f: FlowVar => FlowVar) = this
  override def toString = "goto " + lbl
}
case class FlowGotoCond(x: FlowVar, lbl1: FlowLabel, lbl2: FlowLabel) extends FlowJumpStmt {
  def readVars = Seq(x)
  def mapReadVars(f: FlowVar => FlowVar) = FlowGotoCond(f(x), lbl1, lbl2)
  override def toString = "if " + x + " goto " + lbl1 + " else " + lbl2
}
case class FlowReturn(x: FlowVar) extends FlowJumpStmt {
  def readVars = Seq(x)
  def mapReadVars(f: FlowVar => FlowVar) = FlowReturn(f(x))
  override def toString = "return " + x
}
