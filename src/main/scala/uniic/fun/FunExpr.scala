package uniic.fun
import uniic.misc._
import uniic.types._

sealed trait FunExpr {
  def freeVarsSeq: Seq[String] // Includes duplicates
  def freeVars = freeVarsSeq.toSet

  def subst(map: Map[String, FunVar]): FunExpr

  def defaultToString = super.toString
  override def toString = FunExprToIndentedString(this)
  def toShortString = FunExprToShortString(this)
}

sealed trait FunValue extends FunExpr {
  def subst(map: Map[String, FunVar]): FunValue
}

sealed trait FunLiteral extends FunValue {
  def literalIdentifier: String
}

case object FunUnit extends FunLiteral {
  def freeVarsSeq = Seq.empty
  def subst(map: Map[String, FunVar]) = this
  def literalIdentifier = "$Unit"
}

class FunBool private (val value: Boolean) extends FunLiteral {
  def freeVarsSeq = Seq.empty
  def subst(map: Map[String, FunVar]) = this
  def literalIdentifier = if (value) "$BoolT" else "$BoolF"
}
object FunBool {
  private val TRUE = new FunBool(true)
  private val FALSE = new FunBool(false)
  def apply(value: Boolean): FunBool = if (value) TRUE else FALSE
  def unapply(fb: FunBool): Option[Boolean] = Some(fb.value)
}

case class FunInt(value: Int) extends FunLiteral {
  def freeVarsSeq = Seq.empty
  def subst(map: Map[String, FunVar]) = this
  def literalIdentifier = "$Int" + value
}

case class FunTupleExpr(members: Seq[FunExpr]) extends FunExpr {
  def freeVarsSeq = members.flatMap(_.freeVarsSeq)
  def subst(map: Map[String, FunVar]) = FunTupleExpr(members.map(_.subst(map)))
}

case class FunLambda(params: Seq[(String, TypeAnnotation)], body: FunExpr) extends FunValue {
  def paramNames = params.map(_._1)

  CollectionUtil.findDuplicate(paramNames).map { dup =>
    throw new IllegalArgumentException("Duplicate argument name in lambda: " + dup)
  }

  lazy val freeVarsSeq = body.freeVarsSeq.filterNot(paramNames.contains(_))
  def subst(map: Map[String, FunVar]) = FunLambda(params, body.subst(map -- paramNames))
}
object FunLambda {
  def apply(param: String, body: FunExpr): FunLambda = FunLambda(Seq(param -> MissingTypeAnnotation), body)
  def apply(param: String, ta: TypeAnnotation, body: FunExpr): FunLambda = FunLambda(Seq(param -> ta), body)
}

case class FunBuiltinFunction(name: String, ts: TypeSchema, paramNames: Seq[String], impl: Seq[FunValue] => FunValue) extends FunValue {
  ts.ty.baseType match {
    case TFun(paramTys, _, _) => {
      if (paramTys.size != paramNames.size) {
        throw new IllegalArgumentException("FunBuiltinFunction's type and param name count don't match")
      }
    }
    case _ => throw new IllegalArgumentException("FunBuiltinFunction's type is invalid: " + ts)
  }
  def freeVarsSeq = Seq.empty
  def subst(map: Map[String, FunVar]) = this
  override def toString = name
}

case class FunBuiltinValue(value: Any, ty: Option[BuiltinType]) extends FunValue {
  def freeVarsSeq = Seq.empty
  def subst(map: Map[String, FunVar]) = this
  override def toString = "Builtin(" + value + ")"
}

case class FunVar(name: String) extends FunExpr {
  def freeVarsSeq = Seq(name)
  def subst(map: Map[String, FunVar]) = map.getOrElse(name, this)
}

case class FunApply(fun: FunExpr, args: Seq[FunExpr]) extends FunExpr {
  def freeVarsSeq = fun.freeVarsSeq ++ args.flatMap(_.freeVarsSeq)
  def subst(map: Map[String, FunVar]) = FunApply(fun.subst(map), args.map(_.subst(map)))
}

case class FunLet(bindings: Seq[(String, FunExpr)], body: FunExpr) extends FunExpr {
  def varNames = bindings.map(_._1)

  CollectionUtil.findDuplicate(varNames).map { dup =>
    throw new CompilerException("Duplicate pattern variable in let expression: " + dup)
  }

  lazy val freeVarsSeq = {
    (bindings.flatMap(_._2.freeVarsSeq) ++ body.freeVarsSeq).filterNot(bindings.map(_._1).contains(_))
  }
  def subst(map: Map[String, FunVar]) = {
    val innerMap = map -- bindings.map(_._1)
    val newBindings = bindings.map { case (v, e) => v -> e.subst(innerMap) }
    val newBody = body.subst(innerMap)
    FunLet(newBindings, newBody)
  }
}

object FunLet {
  def maybe(bindings: Seq[(String, FunExpr)], body: FunExpr): FunExpr = {
    if (bindings.isEmpty) body else FunLet(bindings, body)
  }
}

case class FunIfThenElse(cond: FunExpr, thenBody: FunExpr, elseBody: FunExpr) extends FunExpr {
  def freeVarsSeq = cond.freeVarsSeq ++ thenBody.freeVarsSeq ++ elseBody.freeVarsSeq
  def subst(map: Map[String, FunVar]) = FunIfThenElse(cond.subst(map), thenBody.subst(map), elseBody.subst(map))
}

case class FunMatch(head: FunExpr, clauses: Seq[FunCase]) extends FunExpr {
  def freeVarsSeq = head.freeVarsSeq ++ clauses.flatMap(_.freeVarsInOrder)
  def subst(map: Map[String, FunVar]) = {
    FunMatch(head.subst(map), clauses.map(_.subst(map)))
  }

  override def toString = head + " match { " + clauses.mkString("; ") + " }"
}

case class FunCase(pattern: Pattern, body: FunExpr) {
  CollectionUtil.findDuplicate(pattern.patternVarsSeq).map { dup =>
    throw new CompilerException("Duplicate pattern variable in case expression: " + dup)
  }

  def freeVarsInOrder = body.freeVarsSeq.filterNot(pattern.patternVars.contains(_))
  def subst(map: Map[String, FunVar]) = FunCase(pattern, body.subst(map -- pattern.patternVars))
  override def toString = "case " + pattern + " -> " + body
}
