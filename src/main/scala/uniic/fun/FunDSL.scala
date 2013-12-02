package uniic.fun

/** Conveniences for writing expressions in test code. */
trait FunDSL {
  implicit def intToIntConst(x: Int) = FunInt(x)
  implicit def boolToBoolConst(x: Boolean) = FunBool(x)
}

object FunDSL extends FunDSL
