package uniic.types
import uniic.misc._
import uniic.misc.graphviz.GraphvizGraph
import uniic.boolalg._

/** Traditional Hindley-Milner unification augmented with boolean unification.
  *
  * Unifies type equations and ignores type attributes by default.
  */
class TypeUnification extends ExceptionContexts {
  import TypeUnification._

  def unify(
    initialEqs: List[Equation],
    rigidTypeVars: Set[String],
    listener: Listener = Listener.empty
  ): UnificationResult = context("unifying " + firstFewEqs(initialEqs)) {
    val history = new History
    val state = new UnificationState(initialEqs, history)

    verboseComment("Rigid type vars: " + rigidTypeVars)

    var loopCount = 0
    listener.onLoop(loopCount, state.getSubst, state.getEquations)

    while (!state.isDone) {
      state.emitVerboseCommentOfState(rigidTypeVars)

      val eq = state.takeEquation()
      verboseComment("Unifying " + eq)
      unifyOne(eq, rigidTypeVars) match {
        case Some((newSubst, newEqs)) => {
          if (!newEqs.isEmpty) {
            state.addEqs(eq, newEqs)
          }
          if (!newSubst.isEmpty) {
            verboseComment("Unifier fragment generated:\n" + newSubst)
            newSubst.foreach(s => state.addSubst(s, history))
          }
          listener.onLoop(loopCount + 1, state.getSubst, state.getEquations)
        }
        case None => {
          state.addFailedEq(eq)
          listener.onFailedEq(eq)
        }
      }

      loopCount += 1
    }

    verboseComment("Unification processed " + loopCount + " equations")

    val result = new UnificationResult(state.getSubst, history, state.getFailedEquations)
    verboseComment("Result: " + result)
    result
  }

  private class UnificationState(initialEqs: List[Equation], history: History) {
    private val equationPriorityWithCaching = CachingFunction { eq: Equation => equationPriority(eq) }
    private var eqs = initialEqs.sortBy(equationPriorityWithCaching(_))
    private var finalSubst = Seq.empty[KindedSubst]
    private var failedEqs = Seq.empty[Equation]

    sortEqs()

    def getEquations = eqs
    def getSubst = finalSubst
    def getFailedEquations = failedEqs

    def addEqs(sourceEq: Equation, newEqs: List[Equation]) {
      val newEqsSimplified = newEqs.map(_.simplify)
      for (eq <- newEqsSimplified if !eq.isUseless) {
        if (eq.isClearlyUnsolvable) { // Heuristic to fail faster if possible
          addFailedEq(eq)
        } else {
          eqs :+= eq
        }
      }

      sortEqs()
      history.recordEquationSplit(sourceEq, newEqs)
    }

    def addFailedEq(eq: Equation) {
      failedEqs :+= eq
      history.failedEqs += eq
    }

    def addSubst(newSubst: KindedSubst, history: History) {
      eqs = eqs.flatMap(applySubst(newSubst, _))
      failedEqs = failedEqs.flatMap(applySubst(newSubst, _))
      sortEqs()
      finalSubst :+= newSubst
    }

    private def applySubst(subst: KindedSubst, eq: Equation): Option[Equation] = {
      val before = eq
      val after = eq.map(side => subst.applyKinded(side)).simplify
      if (before != after) {
        history.recordSubst(before, subst, after)
      }
      if (after.isUseless) None else Some(after)
    }

    private def sortEqs() {
      eqs = eqs.sortBy(equationPriorityWithCaching(_))
    }

    def isDone = eqs.isEmpty

    def takeEquation(): Equation = {
      val eq = eqs.head
      eqs = eqs.tail
      eq
    }

    def emitVerboseCommentOfState(rigidTypeVars: Set[String]) {
      verboseComment("Equations left:\n  " + eqs.mkString("\n  "))
      lazy val varsLeft = eqs.flatMap(eq => eq._1.freeTLVars ++ eq._2.freeTLVars).toSet
      lazy val freeVarsLeft = varsLeft.filterNot({v: KindedVar => rigidTypeVars.contains(v.name)})
      verboseComment("Free variables left:\n  " + freeVarsLeft.toList.sortBy({v: KindedVar => v.name}).mkString(", ") + "\n\n")
    }
  }

  /** Equations with a lower priority score are eliminated first.
    *
    * We try to choose such an elimination order that boolean equations
    * stay as small as possible.
    *
    * Base type substitutions may generate more attribute equations.
    * We want as many as possible to be available, so we give all base type
    * equations the highest preference.
    *
    * The complexity of boolean unification and simplification depends most
    * strongly on the number of variables in an equation.
    * Ideally we'd like to find such and elimination order that the maximum
    * number of variables in any current or future equation is as small
    * as possible.
    *
    * Of attribute equations, we prefer to substitute simple variables for
    * constants and other variables first, since that is sure to reduce
    * the number of variables.
    *
    * The lowest priority goes to boolean equations that must go through
    * the boolean unification machinery. Our heuristic here is to do
    * equations with the smallest number of variables first in the hopes
    * that this produces smaller substitutions for the remaining equations.
    * For each distinct variable, the boolean unifier approximately doubles
    * the size of the equation, although the simplifier then tries to simplify
    * this intermediate result.
    */
  private def equationPriority(eq: Equation): Int = {
    eq match {
      case Equation(a: AVar, b) if isConstAttr(b) => 0
      case Equation(a, b: AVar) if isConstAttr(a) => 0
      case Equation(a: AVar, b) => 1
      case Equation(a, b: AVar) => 1
      case Equation(a: TypeAttr, b: TypeAttr) => 10000 + (a.freeTLVars ++ b.freeTLVars).size
      case Equation(a, b) => 0
    }
  }

  private def isConstAttr(k: Kinded) = {
    k match {
      case AUniq => true
      case ANonUniq => true
      case _ => false
    }
  }

  private def unifyOne(eq: Equation, rigidTypeVars: Set[String]): Option[(List[KindedSubst], List[Equation])] = {
    def isNotRigid(v: KindedVar) = !rigidTypeVars.contains(v.name)
    eq match {
      case Equation(a, b) if a == b =>
        Some(List.empty, List())

      case Equation(Type(t1, a1), Type(t2, a2)) => {
        Some(List.empty, List(t1 =:= t2, a1 =:= a2))
      }

      case Equation(v@TVar(_), t: BaseType) if !occursFree(v, t) && isNotRigid(v) =>
        Some(List(KindedSubst(v, t)), List())
      case Equation(t: BaseType, v@TVar(_)) if !occursFree(v, t) && isNotRigid(v) =>
        Some(List(KindedSubst(v, t)), List())

      case Equation(v@AVar(_), a: TypeAttr) if !occursFree(v, a) && isNotRigid(v) =>
        Some(List(KindedSubst(v, a)), List())
      case Equation(a: TypeAttr, v@AVar(_)) if !occursFree(v, a) && isNotRigid(v) =>
        Some(List(KindedSubst(v, a)), List())

      case Equation(a1: TypeAttr, a2: TypeAttr) =>
        unifyBoolean(a1.toBoolTerm, a2.toBoolTerm, rigidTypeVars).map(s => (s, List.empty))

      case Equation(TFun(args1, ca1, ret1), TFun(args2, ca2, ret2)) if args1.size == args2.size && args1.map(_.mode) == args2.map(_.mode) => {
        Some(List.empty, List(ret1 =:= ret2, ca1 =:= ca2) ++ (args1.map(_.ty) zip args2.map(_.ty)).map(Function.tupled(_ =:= _)))
      }

      case Equation(TTuple(ts1), TTuple(ts2)) =>
        Some(List.empty, (ts1 zip ts2 map { case (a, b) => a =:= b }).toList)

      case _ => {
        None
      }
    }
  }

  private def occursFree(v: KindedVar, subject: Kinded): Boolean = {
    subject.freeTLVars.contains(v)
  }

  private def unifyBoolean(bt1: BoolTerm, bt2: BoolTerm, rigidTypeVars: Set[String]): Option[List[KindedSubst]] = {
    val fvs = (bt1.variables ++ bt2.variables).toSet filter { v => !rigidTypeVars.contains(v.name) }
    BoolUnification.unify(bt1, bt2, fvs) match {
      case Some(unifier) => {
        val unifierOnAttrs = unifier.map {
          case (BVar(v), r) => AVarSubst(AVar(v), TypeAttr.fromBoolTerm(r))
        }
        Some(unifierOnAttrs.toList)
      }
      case None => None
    }
  }
}


object TypeUnification extends TypeUnification with HasDebugVersion[TypeUnification] {
  val withDebug = new TypeUnification with DebugContexts

  /** A unification equation on base types. Attributes are ignored. */
  case class Equation(_1: Rank1Kinded, _2: Rank1Kinded) extends Product2[Rank1Kinded, Rank1Kinded] {
    def map(f: Rank1Kinded => Rank1Kinded) = Equation(f(_1), f(_2))
    def simplify = map(Kinded.Simplifier.applyKinded(_))
    def isUseless = _1 == _2
    def isClearlyUnsolvable = {
      val c1 = _1.getClass()
      val c2 = _2.getClass()
      val unclearClasses = Seq(classOf[KindedVar], classOf[AOr], classOf[AAnd], classOf[ANot])
      def isUnclear(c: Class[_]) = unclearClasses.exists(uc => uc.isAssignableFrom(c))
      c1 != c2 && !isUnclear(c1) && !isUnclear(c2)
    }
    override def toString = _1 + " =:= " + _2
  }

  /** Provides the syntax `(a =:= b)`. */
  implicit class EquationSyntax(a: Rank1Kinded) {
    def =:=(b: Rank1Kinded) = Equation(a, b)
  }

  class UnificationResult private[TypeUnification] (
    val subst: Seq[KindedSubst],
    val history: History,
    val failedEqs: Seq[Equation]
  ) {
    def substTr = TypeTransformationSeq(subst)
    def isSuccessful = failedEqs.isEmpty
    def substTrIfSuccessful = {
      throwIfUnsuccessful()
      substTr
    }

    def throwIfUnsuccessful() {
      if (!isSuccessful) {
        UnificationResult.throwTypeErrorOnFailedEquations(failedEqs)
      }
    }

    override def toString = {
      if (isSuccessful) {
        subst.mkString("UnificationResult(<OK>, [", ", ", "])")
      } else {
        subst.mkString("UnificationResult(<FAIL: " + failedEqs.mkString(", ") + ">, [", ", ", "])")
      }
    }
  }

  object UnificationResult {
    def throwTypeErrorOnFailedEquations[T](failedEqs: Seq[Equation]): T = {
      throw new TypeError("Failed to unify: " + firstFewEqs(failedEqs))
    }
  }

  private[TypeUnification] def firstFewEqs(eqs: Seq[Equation]) = {
    eqs.take(5).mkString(", ") + (if (eqs.size > 5) ", ..." else "")
  }


  class History {
    sealed trait Record {
      def from: Equation
      def to: Equation
      def label: String = ""
    }
    case class SplitRecord(from: Equation, to: Equation) extends Record
    case class SubstRecord(from: Equation, subst: KindedSubst, to: Equation) extends Record {
      override def label = subst.toString
    }

    var records = Seq.empty[Record]
    var failedEqs = Set.empty[Equation]

    def recordEquationSplit(from: Equation, tos: Seq[Equation]) {
      for (to <- tos) {
        addRecord(SplitRecord(from, to))
      }
    }
    def recordSubst(from: Equation, subst: KindedSubst, to: Equation) {
      addRecord(SubstRecord(from, subst, to))
    }
    def addRecord(r: Record) {
      if (!records.contains(r)) {
        records :+= r
      }
    }
    def toGraphviz: GraphvizGraph = {
      val gg = new GraphvizGraph
      val allNodeObjects = CollectionUtil.uniq(records.map(_.from) ++ records.map(_.to))
      var nodeMap = Map.empty[AnyRef, GraphvizGraph.Node]
      for (nobj <- allNodeObjects) {
        val node = gg.addNode(nobj.toString)
        node.attributes("shape") = "none"
        if (failedEqs.contains(nobj)) {
          node.attributes("fontcolor") = "red"
        }
        nodeMap += (nobj -> node)
      }
      for ((r, i) <- records.zipWithIndex) {
        val edge = gg.addEdge(nodeMap(r.from), nodeMap(r.to))
        edge.attributes("label") = i + ". " + r.label
        edge.attributes("fontcolor") = "#7F7F7F"
      }
      gg
    }
  }

  trait Listener {
    def onLoop(iter: Int, substs: Seq[KindedSubst], remainingEqs: List[Equation]) {}
    def onFailedEq(eq: Equation) {}
  }
  object Listener {
    val empty = new Listener {}
  }
}
