package uniic.types

trait TypeTransformation {
  def applyKinded(k: Rank1Kinded): Rank1Kinded = {
    k match {
      case t: Type => applyType(t)
      case bt: BaseType => applyBaseType(bt)
      case a: TypeAttr => applyTypeAttr(a)
    }
  }

  def applyType(t: Type): Type = {
    Type(applyBaseType(t.baseType), applyTypeAttr(t.attr))
  }

  def applyBaseType(bt: BaseType): BaseType = {
    bt match {
      case TUnit => bt
      case TInt => bt
      case TBool => bt
      case TInternal => bt
      case TVar(_) => bt
      case TTuple(members) => TTuple(members.map(applyType))
      case BuiltinType(_) => bt
      case TFun(params, closureAttr, returnType) => {
        TFun(params.map(applyTFunParam), applyTypeAttr(closureAttr), applyType(returnType))
      }
    }
  }

  def applyTFunParam(p: TFunParam): TFunParam = {
    p match {
      case TFunParam(ty, mode) => TFunParam(applyType(ty), mode)
    }
  }

  def applyTypeAttr(a: TypeAttr): TypeAttr = {
    a match {
      case AOr(left, right) => AOr(applyTypeAttr(left), applyTypeAttr(right))
      case AAnd(left, right) => AAnd(applyTypeAttr(left), applyTypeAttr(right))
      case ANot(operand) => ANot(applyTypeAttr(operand))
      case _ => a
    }
  }
}

class TypeTransformationSeq(val trs: Seq[TypeTransformation]) extends TypeTransformation {
  override def applyKinded(k: Rank1Kinded) = trs.foldLeft(k) { case (x, s) => s.applyKinded(x) }
  override def applyType(t: Type): Type = trs.foldLeft(t) { case (x, s) => s.applyType(x) }
  override def applyBaseType(bt: BaseType): BaseType = trs.foldLeft(bt) { case (x, s) => s.applyBaseType(x) }
  override def applyTFunParam(p: TFunParam): TFunParam = trs.foldLeft(p) { case (x, s) => s.applyTFunParam(x) }
  override def applyTypeAttr(a: TypeAttr): TypeAttr = trs.foldLeft(a) { case (x, s) => s.applyTypeAttr(x) }

  override def toString = trs.mkString("[", ", ", "]")

  def ++(that: TypeTransformationSeq) = TypeTransformationSeq(this.trs ++ that.trs)

  def toSeq = trs
}

object TypeTransformationSeq {
  def apply(trs: Seq[TypeTransformation]) = new TypeTransformationSeq(trs)
}
