package uniic.types
import uniic.misc.CollectionUtil

/** An unevaluated type. */
abstract class TypeExpr {
  def apply(attr: TypeExpr) = TEAttribution(this, attr)
  def withAttr(attr: TypeExpr) = TEAttribution(this, attr)
  def freeTLVars: Set[String]
  def subst(v: TEVar, r: TypeExpr): TypeExpr
}

case object TEUnit extends TypeExpr {
  override def toString = "()"
  def freeTLVars = Set.empty
  def subst(v: TEVar, r: TypeExpr) = this
}
case class TEVar(name: String) extends TypeExpr {
  override def toString = name
  def freeTLVars = Set(name)
  def subst(v: TEVar, r: TypeExpr) = if (v == this) r else this
}

case class TETuple(members: Seq[TypeExpr]) extends TypeExpr {
  override def toString = "(" + members.mkString(", ") + ")"
  def freeTLVars = members.flatMap(_.freeTLVars).toSet
  def subst(v: TEVar, r: TypeExpr) = TETuple(members.map(_.subst(v, r)))
}

case class TESchema(vars: Seq[(String, Kind)], body: TypeExpr) extends TypeExpr {
  override def toString = {
    "(forall " +
    vars.map {case (name, kind) => name + ":" + kind}.mkString(" ") +
    ". " + body + ")"
  }

  def freeTLVars = body.freeTLVars -- vars.map(_._1)
  def subst(v: TEVar, r: TypeExpr) = {
    if (!vars.exists(_._1 == v.name)) {
      TESchema(vars, body.subst(v, r))
    } else {
      this
    }
  }
}

case class TEFunParam(ty: TypeExpr, mode: ParamMode) {
  override def toString = mode + " " + ty
  def freeTLVars = ty.freeTLVars
}
case class TEFun(params: Seq[TEFunParam], closureAttr: TypeExpr, returnType: TypeExpr) extends TypeExpr {
  override def toString = "(" + params.mkString(", ") + " | " + closureAttr + " => " + returnType + ")"
  def freeTLVars = params.flatMap(_.freeTLVars).toSet ++ returnType.freeTLVars
  def subst(v: TEVar, r: TypeExpr) = TEFun(params.map {
    case TEFunParam(ty, mode) => TEFunParam(ty.subst(v, r), mode)
  }, closureAttr.subst(v, r), returnType.subst(v, r))
}
object TEFun {
  def apply(param1Type: TypeExpr, returnType: TypeExpr): TEFun = apply(Seq(TEFunParam(param1Type, MNone)), TENonUniq, returnType)
  def apply(param1Type: TEFunParam, returnType: TypeExpr): TEFun = apply(Seq(param1Type), TENonUniq, returnType)
  def apply(params: Seq[TEFunParam], returnType: TypeExpr): TEFun = apply(params, TENonUniq, returnType)
}

case class TEAttribution(base: TypeExpr, attr: TypeExpr) extends TypeExpr {
  override def toString = base + "^" + attr
  def freeTLVars = base.freeTLVars ++ attr.freeTLVars
  def subst(v: TEVar, r: TypeExpr) = TEAttribution(base.subst(v, r), attr.subst(v, r))
}

case object TEUniq extends TypeExpr {
  override def toString = "*"
  def freeTLVars = Set.empty
  def subst(v: TEVar, r: TypeExpr) = this
}
case object TENonUniq extends TypeExpr {
  override def toString = "+"
  def freeTLVars = Set.empty
  def subst(v: TEVar, r: TypeExpr) = this
}
case class TEOr(left: TypeExpr, right: TypeExpr) extends TypeExpr {
  override def toString = "(" + left + """ \/ """ + right + ")"
  def freeTLVars = left.freeTLVars ++ right.freeTLVars
  def subst(v: TEVar, r: TypeExpr) = TEOr(left.subst(v, r), right.subst(v, r))
}
case class TEAnd(left: TypeExpr, right: TypeExpr) extends TypeExpr {
  override def toString = "(" + left + """ /\ """ + right + ")"
  def freeTLVars = left.freeTLVars ++ right.freeTLVars
  def subst(v: TEVar, r: TypeExpr) = TEAnd(left.subst(v, r), right.subst(v, r))
}
case class TENot(operand: TypeExpr) extends TypeExpr {
  override def toString = "~" + operand.toString
  def freeTLVars = operand.freeTLVars
  def subst(v: TEVar, r: TypeExpr) = TENot(operand.subst(v, r))
}
