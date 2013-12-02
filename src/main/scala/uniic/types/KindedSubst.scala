package uniic.types

import scala.language.existentials
import uniic.misc.CollectionUtil

object KindedSubst {
  def apply(toReplace: TVar, replacement: BaseType) = TVarSubst(toReplace, replacement)
  def apply(toReplace: AVar, replacement: TypeAttr) = AVarSubst(toReplace, replacement)
  def apply(toReplace: Type, replacement: Type) = TypeSubst(toReplace, replacement)
  def apply(substs: Seq[KindedSubst]) = TypeTransformationSeq(substs)
}

/** A type-level substitution. */
trait KindedSubst extends TypeTransformation {
  val toReplace: Kinded
  val replacement: Kinded
  override def toString = toReplace + " := " + replacement

  protected def replacesVariable(v: String): Boolean

  def applyTypeSchema(ts: TypeSchema) = {
    if (ts.vars.exists(v => this.replacesVariable(v.name))) ts
    else if (ts.vars.contains(replacement)) throw new IllegalArgumentException(s"Unhygenic substitution of $replacement into $ts")
    else TypeSchema(ts.vars, applyType(ts.ty))
  }
}

case class TVarSubst(toReplace: TVar, replacement: BaseType) extends KindedSubst {
  override def applyBaseType(bt: BaseType) = {
    if (bt == toReplace) replacement
    else super.applyBaseType(bt)
  }

  protected def replacesVariable(v: String) = toReplace.name == v
}

case class AVarSubst(toReplace: AVar, replacement: TypeAttr) extends KindedSubst {
  override def applyTypeAttr(a: TypeAttr) = {
    if (a == toReplace) replacement
    else super.applyTypeAttr(a)
  }

  protected def replacesVariable(v: String) = toReplace.name == v
}

case class TypeSubst(toReplace: Type, replacement: Type) extends KindedSubst {
  override def applyType(t: Type) = {
    if (t == toReplace) replacement
    else super.applyType(t)
  }

  protected def replacesVariable(v: String) = toReplace.baseType == TVar(v)
}

case class VarToVarSubst[T <: KindedVar](toReplace: T, replacement: T) extends KindedSubst {
  override def applyBaseType(bt: BaseType) = {
    if (bt == toReplace) replacement.asInstanceOf[TVar]
    else super.applyBaseType(bt)
  }
  override def applyTypeAttr(a: TypeAttr) = {
    if (a == toReplace) replacement.asInstanceOf[AVar]
    else super.applyTypeAttr(a)
  }

  protected def replacesVariable(v: String) = toReplace.name == v

  def reverse = VarToVarSubst[T](replacement, toReplace)

  def asTuple: (KindedVar, KindedVar) = (toReplace, replacement)
}

case class VarToVarSubstSeq(val varToVarSubsts: Seq[VarToVarSubst[T] forSome { type T <: KindedVar }])
  extends TypeTransformationSeq(varToVarSubsts)
  with Function[KindedVar, KindedVar]
{
  val sourceVars: Seq[KindedVar] = varToVarSubsts.map(_.toReplace)
  val targetVars: Seq[KindedVar] = varToVarSubsts.map(_.replacement)

  def reverse = VarToVarSubstSeq(varToVarSubsts.map(_.reverse))

  def apply(v: KindedVar) = varToVarSubsts.find(_.toReplace == v).map(_.replacement).getOrElse(v)

  override def toString =
    "VarToVarSubstSeq(" +
    CollectionUtil.pairsToPrettyString(varToVarSubsts.map(_.asTuple), sort = false, sep = " ") +
    ")"
}
