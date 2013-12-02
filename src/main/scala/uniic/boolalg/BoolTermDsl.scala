package uniic.boolalg

trait BoolTermDsl {
  /** Enables the following syntax:
    * {{{
    * - `!A`
    * - `A /\ B`
    * - `A \/ B`
    * - `A --> B`
    * - `A <--> B`
    * }}} */
  implicit def boolOpsSyntax(left: BoolTerm) = new {
    def unary_!(): BoolTerm = BNot(left)
    def /\(right: BoolTerm): BoolTerm = BAnd(left, right)
    def \/(right: BoolTerm): BoolTerm = BOr(left, right)
    def -->(right: BoolTerm): BoolTerm = BOr(right, BNot(left))
    def <-->(right: BoolTerm): BoolTerm = BOr(BAnd(left, right), BAnd(BNot(left), BNot(right)))
  }

  /** Adds the method `X.ifElse(A, B)`. */
  implicit def ifElse(cond: BoolTerm) = new {
    def ifElse(thenClause: BoolTerm, elseClause: BoolTerm) =
      (cond /\ thenClause) \/ (!cond /\ elseClause)
  }

  /** Enables the use of `"x"` instead of `BVar("x")`. */
  object StringsAsBVars {
    implicit def stringToBVar(x: String) = BVar(x)
    implicit def stringBoolOpsSyntax(x: String) = boolOpsSyntax(BVar(x))
    implicit def stringIfElse(x: String) = ifElse(BVar(x))

    def mkAssignment(map: (String, Boolean)*): BVar => Boolean = {
      Map(map.map { case (x, b) => (BVar(x), b) }: _*)
    }
  }
}

object BoolTermDsl extends BoolTermDsl
