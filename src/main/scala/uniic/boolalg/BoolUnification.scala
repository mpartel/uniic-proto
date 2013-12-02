package uniic.boolalg
import uniic.misc.Unicode

/** Solves boolean equations.
  *
  * This implements the 'successive variable elimination' algorithm as
  * described in de Vries' PhD, section 2.2.4. */
object BoolUnification extends BoolTermDsl {
  type Unifier = Seq[(BVar, BoolTerm)]
  object Unifier {
    val empty = Seq.empty[(BVar, BoolTerm)]
  }

  /** Attempts to find the most general unifier for `t1 == t2` */
  def unify(t1: BoolTerm, t2: BoolTerm, freeVars: Set[BVar]): Option[Unifier] = {
    val eq0 = t1.ifElse(!t2, t2).simplify
    val (unifier, consistencyCondition) = unify0(eq0, freeVars.toList.sortBy(_.name))

    assert(consistencyCondition.variables.intersect(freeVars).isEmpty)

    if (BNot(consistencyCondition).isTautology) {
      Some(unifier.filterNot(p => p._1 == p._2))
    } else {
      None
    }
  }

  /** Finds a most general unifier for `t == 0` and a ground term describing success.
    *
    * Let `t0 = t[x <- 0]` and `t1 = t[x <- 1]`.
    * For each variable `x` an equation `t0 /\ t1 == 0` is made.
    * When a unifier `S` for this equation is found, a unifier for `t == 0` can be
    * constructed as `S o [x <- t0 \/ (x /\ !t1)]`.
    *
    * Here is an attempt at an intuitive explanation.
    * If we can solve `t0 /\ t1 == 0` with substitution `S` then adding
    * either `[x <- 0]` or `[x <- 1]` to `S` will make it solve `t == 0`.
    *
    * Note that `t <=> x.ifElse(t1, t0)`.
    * Now substituting `t[x <- t0 \/ (x /\ !t1)]` makes the equation
    * `(t0 \/ (x /\ !t1)).ifElse(t1, t0) == 0`, which is equivalent to
    * `(!x /\ !t0 /\ t1)  \/  (x /\ t0 /\ !t1) == 1`.
    * This expresses that `x` must be chosen so that ''only'' the opposite
    * choice would make `t == 1`. Now
    * `(!x /\ !t0 /\ t1)  \/  (x /\ t0 /\ !t1) == 1`  =>
    * `x.ifElse(t0 /\ !t1, !t0 /\ t1) == 1`           =>
    * `x.ifElse(!t0 /\ t1, t0 /\ !t1) == 0`           =>
    * `x.ifElse(t1, t0) == 0`                         =>
    * `t == 0`.
    *
    * Thus the substitution `[x <- t0 \/ (x /\ !t1)]` solves
    * `t == 0` if the remaining variables are substituted correctly.
    * When no variables remain, what is left is a ground term that must also
    * be equivalent to `0` for the substitution to have solved the original equation.
    *
    * Why the substitution obtained this way is the most general solution is shown
    * in de Vries' PhD and its references. */
  private def unify0(t: BoolTerm, vars: List[BVar]): (Unifier, BoolTerm) = {
    vars match {
      case Nil => (Unifier.empty, t)
      case x :: xs => {
        val t0 = t.subst(x, BFalse)
        val t1 = t.subst(x, BTrue)
        val e = (t0 /\ t1).simplify
        val (s, cc) = unify0(e, xs)
        ((x, (t0 \/ (x /\ !t1)).simplify) +: s, cc)
      }
    }
  }

}