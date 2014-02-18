package uniic.boolalg
import scala.math.max
import uniic.misc.IterativeAlgo

abstract sealed class BoolTerm {
  def depth: Int
  def size: Int

  def subst(pairs: Seq[(BVar, BoolTerm)]): BoolTerm = {
    pairs.foldLeft(this){ case (t, (v, r)) => t.subst(v, r) }
  }

  def subst(v: BVar, r: BoolTerm): BoolTerm

  def variables: Set[BVar] = {
    this match {
      case v@BVar(_) => Set(v)
      case BAnd(l, r) => l.variables ++ r.variables
      case BOr(l, r) => l.variables ++ r.variables
      case BNot(x) => x.variables
      case BTrue => Set.empty
      case BFalse => Set.empty
    }
  }

  def map(f: BoolTerm => BoolTerm): BoolTerm = {
    this match {
      case v@BVar(_) => f(v)
      case BAnd(l, r) => f(BAnd(l.map(f), r.map(f)))
      case BOr(l, r) => f(BOr(l.map(f), r.map(f)))
      case BNot(x) => f(BNot(x.map(f)))
      case BTrue => f(BTrue)
      case BFalse => f(BFalse)
    }
  }

  def eval(assignment: BVar => Boolean): Boolean = {
    this match {
      case BTrue => true
      case BFalse => false
      case BAnd(l, r) => l.eval(assignment) && r.eval(assignment)
      case BOr(l, r) => l.eval(assignment) || r.eval(assignment)
      case BNot(s) => !s.eval(assignment)
      case v@BVar(_) => assignment(v)
    }
  }

  def simplify: BoolTerm = BoolMinimizer(this.quickSimplify)

  def quickSimplify: BoolTerm = { // Only cleans up the most obvious messes
    def simplify1(t: BoolTerm): BoolTerm = {
      t match {
        case BAnd(BFalse, _) => BFalse
        case BAnd(_, BFalse) => BFalse
        case BAnd(BTrue, r) => r
        case BAnd(l, BTrue) => l
        case BAnd(l, r) if l == r => l
        case BAnd(l, r) if l == BNot(r) || BNot(l) == r => BFalse
        case BAnd(l, r) => BAnd(simplify1(l), simplify1(r))

        case BOr(BTrue, _) => BTrue
        case BOr(_, BTrue) => BTrue
        case BOr(BFalse, r) => r
        case BOr(l, BFalse) => l
        case BOr(l, r) if l == r => l
        case BOr(l, r) if l == BNot(r) || BNot(l) == r => BTrue
        case BOr(l, r) => BOr(simplify1(l), simplify1(r))

        case BNot(BFalse) => BTrue
        case BNot(BTrue) => BFalse
        case BNot(BNot(x)) => x
        case BNot(x) => BNot(simplify1(x))

        case _ => t
      }
    }

    IterativeAlgo.untilNoChange(this)(simplify1)
  }

  def isTautology: Boolean = {
    val vars = this.variables.toList.sortBy(_.name)

    if (vars.size > 24) {
      throw new IllegalArgumentException("Too many variables")
    }

    if (vars.size > 0) {
      val varToIndex: Function[BVar, Int] = vars.zipWithIndex.toMap

      def assignmentForIndex(i: Int) = { v: BVar =>
        val bit = varToIndex(v)
        ((i >> bit) & 1) == 1
      }

      val combinations = (1 << vars.size)
      (0 until combinations).forall { i =>
        this.eval(assignmentForIndex(i)) == true
      }
    } else {
      this.eval(_ => throw new AssertionError("no vars expected"))
    }
  }
}


case object BTrue extends BoolTerm {
  def depth: Int = 0
  def size: Int = 1
  def subst(v: BVar, r: BoolTerm) = this
  override def toString = "1"
}

case object BFalse extends BoolTerm {
  def depth = 0
  def size = 1
  def subst(v: BVar, r: BoolTerm) = this
  override def toString = "0"
}

case class BVar(name: String) extends BoolTerm {
  def depth = 0
  def size = 1
  def subst(v: BVar, r: BoolTerm) = if (v == this) r else this
  override def toString = name
}

case class BAnd(left: BoolTerm, right: BoolTerm) extends BoolTerm {
  lazy val depth = max(left.depth, right.depth) + 1
  def size = left.size + right.size + 1
  def subst(v: BVar, r: BoolTerm) = BAnd(left.subst(v, r), right.subst(v, r))
  override def toString = "(" + left + " ∧ " + right + ")"
}

case class BOr(left: BoolTerm, right: BoolTerm) extends BoolTerm {
  lazy val depth = max(left.depth, right.depth) + 1
  def size = left.size + right.size + 1
  def subst(v: BVar, r: BoolTerm) = BOr(left.subst(v, r), right.subst(v, r))
  override def toString = "(" + left + " ∨ " + right + ")"
}

case class BNot(subject: BoolTerm) extends BoolTerm {
  lazy val depth = subject.depth + 1
  def size = subject.size + 1
  def subst(v: BVar, r: BoolTerm) = BNot(subject.subst(v, r))
  override def toString = "¬" + subject
}
