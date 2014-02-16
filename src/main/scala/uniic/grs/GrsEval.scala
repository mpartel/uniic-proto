package uniic.grs

import uniic.misc.CompilerException

trait GrsEval {
  class StuckException(val msg: String) extends CompilerException(msg)
  class TimeLimitExceededException extends CompilerException("Maximum evaluation steps exceeded")

  sealed trait TimeLimit
  case object NoTimeLimit extends TimeLimit
  case class MaxSteps(steps: Int) extends TimeLimit

  def eval(grs: Grs, debug: Boolean = false)(implicit timeLimit: TimeLimit = NoTimeLimit): Grs
}

/** The default evaluation strategy. */
object GrsEval extends GrsEvalCallByValue
