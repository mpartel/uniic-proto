package uniic.boolalg
import uniic.misc.IterativeAlgo
import uniic.misc.Unicode
import scala.util.Sorting
import java.util.Arrays

/** A boolean expression in disjunctive normal form.
  *
  * Represented as an array of clauses where each clause
  * is represented as an array of bytes. The array indices
  * correspond to the indices in the variables array and
  * the bytes are
  * 1 if the variable is a literal in the clause,
  * 0 if the variable is a negated literal in the clause and
  * 2 (DontCare) of the variable is not in the clause. */
final class BoolDnf(val vars: Array[String], val clauses: Array[Array[Byte]]) {
  import BoolDnf.DontCare

  def toBoolTerm: BoolTerm = {
    if (!clauses.isEmpty) {
      clauses map { clauseToBoolTerm(_) } reduce { BOr(_, _) }
    } else {
      BFalse
    }
  }

  private def clauseToBoolTerm(bytes: Array[Byte]): BoolTerm = {
    var literals = List.empty[BoolTerm]
    for (i <- 0 until vars.length) {
      bytes(i) match {
        case 0 => { literals ::= BNot(BVar(vars(i))) }
        case 1 => { literals ::= BVar(vars(i)) }
        case DontCare => { }
      }
    }
    literals = literals.sortBy {l => // Sorting makes the term look nicer
      (l: @unchecked) match {
        case BNot(BVar(name)) => name
        case BVar(name) => name
      }
    }
    if (literals.isEmpty) {
      BTrue
    } else {
      literals reduce { BAnd(_, _) }
    }
  }

  override def equals(other: Any) = {
    other match {
      case that: BoolDnf => {
        (this.vars.toSet == that.vars.toSet) &&
        (this.clauses.forall(a => that.clauses.exists(_.deep.equals(a.deep)))) &&
        (that.clauses.forall(a => this.clauses.exists(_.deep.equals(a.deep))))
      }
      case _ => false
    }
  }

  override def hashCode = 0 // todo if needed

  override def toString = {
    clauses map {clause =>
      var literals = List.empty[String]
      for (i <- 0 until vars.length) {
        clause(i) match {
          case 0 => literals ::= Unicode.negationSymbol + vars(i)
          case 1 => literals ::= vars(i)
          case DontCare =>
        }
      }
      literals = literals.sorted
      "(" + (literals mkString (" " + Unicode.conjunctionSymbol + " ")) + ")"
    } mkString (" " + Unicode.disjunctionSymbol + " ")
  }
}

/** Converts boolean expressions to DNF form.
  *
  * Returns None if the DNF has a clause that is trivially true. */
object BoolDnf {
  val DontCare: Byte = 2 // Upper case so it can be used in pattern matches

  def apply(t: BoolTerm): Option[BoolDnf] = {
    val vars = t.variables.map(_.name).toArray
    val dnfTerm = pushDownConjunctions(pushDownNegations(t))
    val clauses = findClauses(dnfTerm)
    if (clauses.exists(isTriviallyTrue(_))) {
      None
    } else {
      val satisfiableClauses = clauses.filterNot(isTriviallyFalse(_))
      val compressedClauses = satisfiableClauses.map(compressClause(vars, _)).toArray
      Some(new BoolDnf(vars, compressedClauses))
    }
  }

  private def pushDownNegations(t: BoolTerm): BoolTerm = {
    t match {
      case BNot(BAnd(l, r)) => BOr(pushDownNegations(BNot(l)), pushDownNegations(BNot(r)))
      case BNot(BOr(l, r)) => BAnd(pushDownNegations(BNot(l)), pushDownNegations(BNot(r)))
      case BNot(BNot(x)) => pushDownNegations(x)
      case BNot(BTrue) => BFalse
      case BNot(BFalse) => BTrue
      case BNot(BVar(_)) => t
      case BAnd(l, r) => BAnd(pushDownNegations(l), pushDownNegations(r))
      case BOr(l, r) => BOr(pushDownNegations(l), pushDownNegations(r))
      case BTrue => t
      case BFalse => t
      case BVar(_) => t
    }
  }

  private def pushDownConjunctions(t: BoolTerm): BoolTerm = {
    IterativeAlgo.untilNoChange(t) { tt: BoolTerm => pushDownConjunctionsOnce(tt) }
  }

  private def pushDownConjunctionsOnce(t: BoolTerm): BoolTerm = {
    t match {
      case BOr(l, r) => BOr(pushDownConjunctions(l), pushDownConjunctions(r))
      case BAnd(BOr(a, b), r) => BOr(pushDownConjunctions(BAnd(a, r)), pushDownConjunctions(BAnd(b, r)))
      case BAnd(l, BOr(a, b)) => BOr(pushDownConjunctions(BAnd(l, a)), pushDownConjunctions(BAnd(l, b)))
      case BAnd(l, r) => BAnd(pushDownConjunctions(l), pushDownConjunctions(r)) // May require iteration
      case BNot(_) => t // Assume negations already pushed down
      case BTrue => t
      case BFalse => t
      case BVar(_) => t
    }
  }

  private def findClauses(t: BoolTerm): List[List[BoolTerm]] = {
    t match {
      case BOr(l, r) => findClauses(l) ++ findClauses(r)
      case _ => List(createClause(t))
    }
  }

  private def createClause(t: BoolTerm): List[BoolTerm] = {
    t match {
      case BAnd(l, r) => createClause(l) ++ createClause(r)
      case BNot(BTrue) => List(BFalse)
      case BNot(BFalse) => List(BTrue)
      case BNot(_) => List(t)
      case BVar(_) => List(t)
      case BTrue => List(t)
      case BFalse => List(t)
      case BOr(l, r) => throw new AssertionError("Met disjunction " + t + " while creating clause")
    }
  }

  private def isTriviallyTrue(clause: List[BoolTerm]) =
    clause.forall(t => t == BTrue || t == BNot(BFalse))
  private def isTriviallyFalse(clause: List[BoolTerm]) =
    clause.exists(t => t == BFalse || t == BNot(BTrue)) || hasLiteralAndItsNegation(clause)

  private def hasLiteralAndItsNegation(clause: List[BoolTerm]): Boolean = {
    clause match {
      case (x@BVar(_)) :: xs => xs.contains(BNot(x)) || hasLiteralAndItsNegation(xs)
      case (BNot(x@BVar(_))) :: xs => xs.contains(x) || hasLiteralAndItsNegation(xs)
      case _ :: xs => hasLiteralAndItsNegation(xs)
      case Nil => false
    }
  }

  private def compressClause(vars: Array[String], clause: List[BoolTerm]): Array[Byte] = {
    // We assume this is not called with a trivially true or false clause
    var result = Array.fill[Byte](vars.length)(DontCare)
    for (t <- clause) {
      t match {
        case BNot(BVar(name)) => {
          val i = vars.indexOf(name)
          assert(result(i) != 1)
          result(i) = 0
        }
        case BVar(name) => {
          val i = vars.indexOf(name)
          assert(result(i) != 0)
          result(vars.indexOf(name)) = 1
        }
        case BTrue =>
        case BNot(BFalse) =>
        case _ => throw new AssertionError("Met non-literal " + t + " in clause")
      }
    }
    result
  }
}