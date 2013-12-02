package uniic.types
import uniic.misc._
import uniic.boolalg._

trait Kinded {
  def kind: Kind
  def freeTLVars: Set[KindedVar]
  def freeTVars: Set[TVar] = freeTLVars.flatMap {
    case v: TVar => Some(v)
    case _ => None
  }
  def freeAVars: Set[AVar] = freeTLVars.flatMap {
    case v: AVar => Some(v)
    case _ => None
  }

  def subst(toReplace: TVar, replacement: BaseType): Kinded
  def subst(toReplace: AVar, replacement: TypeAttr): Kinded
  def subst(toReplace: Type, replacement: Type): Kinded

  def toVerboseString = toString
}

object Kinded {
  object Simplifier extends TypeTransformation {
    override def applyTypeAttr(a: TypeAttr) = a.simplify
  }
}

sealed trait Rank1Kinded extends Kinded {
  def subst(toReplace: TVar, replacement: BaseType) = TVarSubst(toReplace, replacement).applyKinded(this)
  def subst(toReplace: AVar, replacement: TypeAttr) = AVarSubst(toReplace, replacement).applyKinded(this)
  def subst(toReplace: Type, replacement: Type) = TypeSubst(toReplace, replacement).applyKinded(this)
}

trait KindedVar extends Kinded {
  val name: String
  def freeTLVars = Set(this)
  override def toString = name
}


case class Type(val baseType: BaseType, val attr: TypeAttr) extends Rank1Kinded {
  def param = TFunParam(this, MNone)
  def borrowed = TFunParam(this, MBorrowed)

  def kind = KType
  def freeTLVars = baseType.freeTLVars ++ attr.freeTLVars
  override def toString = {
    baseType match {
      case tf@TFun(params, closureAttrs, returnTy) => {
        "(" + tf.paramListToString + " " +
        tf.arrowToString(Some(attr)) + " " +
        returnTy + ")"
      }
      case _ => "" + baseType + "[" + attr + "]"
    }
  }

  def generalize(rigidTypeVars: Set[String]): TypeSchema = {
    val freeVars = this.freeTLVars.filterNot { v =>
      rigidTypeVars.contains(v.name)
    }.toList.sortBy(_.name)
    TypeSchema(freeVars, this).simplify
  }

  /** Produces a TypeSchema with no generalized variables. */
  def toTrivialTypeSchema: TypeSchema = {
    TypeSchema(Seq(), this)
  }

  override def subst(toReplace: TVar, replacement: BaseType) = TVarSubst(toReplace, replacement).applyType(this)
  override def subst(toReplace: AVar, replacement: TypeAttr) = AVarSubst(toReplace, replacement).applyType(this)
  override def subst(toReplace: Type, replacement: Type) = TypeSubst(toReplace, replacement).applyType(this)
}

abstract class BaseType extends Rank1Kinded {
  def kind = KBaseType

  def withAttr(attr: TypeAttr): Type = Type(this, attr)
  def apply(attr: TypeAttr): Type = this.withAttr(attr)
  def apply(varName: String): Type = this.withAttr(AVar(varName))
  def uniq: Type = withAttr(AUniq)
  def nonUniq: Type = withAttr(ANonUniq)

  override def subst(toReplace: TVar, replacement: BaseType) = TVarSubst(toReplace, replacement).applyBaseType(this)
  override def subst(toReplace: AVar, replacement: TypeAttr) = AVarSubst(toReplace, replacement).applyBaseType(this)
  override def subst(toReplace: Type, replacement: Type) = TypeSubst(toReplace, replacement).applyBaseType(this)
}

case object TUnit extends BaseType {
  def freeTLVars = Set.empty
  override def toString = "Unit"
}
case object TInt extends BaseType {
  def freeTLVars = Set.empty
  override def toString = "Int"
}
case object TBool extends BaseType {
  def freeTLVars = Set.empty
  override def toString = "Bool"
}
/** Placehilder for internal constructs that aren't visible to the language user. */
case object TInternal extends BaseType {
  def freeTLVars = Set.empty
  override def toString = "Internal"
}

case class TVar(name: String) extends BaseType with KindedVar

case class TTuple(members: Seq[Type]) extends BaseType {
  members.length match {
    case 0 => throw new CompilerException("Internal error: Attempt to construct nullary tuple type (use TUnit instead).")
    case 1 => throw new CompilerException("Internal error: Attempt to construct unary tuple type.")
    case _ =>
  }
  def freeTLVars = members.flatMap(_.freeTLVars).toSet
  override def toString = "(" + members.mkString(", ") + ")"
}

/** Opaque builtin types. */
case class BuiltinType(name: String) extends BaseType {
  def freeTLVars = Set.empty
  override def toString = name
}


sealed trait ParamMode {
  def isBorrowing: Boolean
  def requiresUniqueness: Boolean
}
case object MNone extends ParamMode {
  def isBorrowing = false
  def requiresUniqueness = false
}
case object MBorrowed extends ParamMode {
  def isBorrowing = true
  def requiresUniqueness = true
  def symbol = "@"
}

case class TFunParam(ty: Type, mode: ParamMode) {
  override def toString = mode match {
    case MNone => ty.toString
    case MBorrowed => MBorrowed.symbol + ty
  }
}

/** The type of a function.
 *
 * See `TypeLevelFun` for a function from types to types. */
case class TFun(params: Seq[TFunParam], closureAttr: TypeAttr, returnType: Type) extends BaseType {
  def freeTLVars = params.map(_.ty.freeTLVars).fold(Set())(_ ++ _) ++ returnType.freeTLVars

  def paramListToString = {
    if (params.size == 1) params(0)
    else params.mkString("(", ", ", ")")
  }
  def arrowToString(mainAttr: Option[TypeAttr]) = {
    if (mainAttr.isEmpty && closureAttr == ANonUniq) {
      Unicode.rightArrowSymbol
    } else {
      val attrs = (mainAttr, closureAttr) match {
        case (Some(a), ANonUniq) => a.toString
        case (Some(a), ca) => a + "|" + ca
        case (None, ca) => "|" + ca
      }
      "-[" + attrs + "]" + Unicode.rightArrowSymbol
    }
  }
  override def toString = {
    "(" + paramListToString + " " + arrowToString(None) + " " + returnType + ")"
  }

  lazy val borrowedParams = params.filter(p => p.mode.isBorrowing)
  lazy val borrowedParamIndices = params.indices.filter(i => params(i).mode.isBorrowing)
  lazy val borrowedParamTypes = borrowedParams.map(_.ty)
}

object TFun {
  def apply(param1Type: Type, returnType: Type): TFun = apply(Seq(TFunParam(param1Type, MNone)), ANonUniq, returnType)
  def apply(param1Type: TFunParam, returnType: Type): TFun = apply(Seq(param1Type), ANonUniq, returnType)
}


abstract class TypeAttr extends Rank1Kinded with Ordered[TypeAttr] {
  def kind = KTypeAttr
  def compare(that: TypeAttr) = (this, that) match {
    case (a, b) if a == b => 0
    case (a: AVar, b: AVar) => a.name.compareTo(b.name)
    case (a: AVar, _) => 1
    case (AUniq, _) => -1
    case (ANonUniq, AUniq) => 1
    case (ANonUniq, _) => -1
  }

  def toBoolTerm: BoolTerm

  def simplify: TypeAttr = TypeAttr.fromBoolTerm(this.toBoolTerm.simplify)

  override def subst(toReplace: TVar, replacement: BaseType) = TVarSubst(toReplace, replacement).applyTypeAttr(this)
  override def subst(toReplace: AVar, replacement: TypeAttr) = AVarSubst(toReplace, replacement).applyTypeAttr(this)
  override def subst(toReplace: Type, replacement: Type) = TypeSubst(toReplace, replacement).applyTypeAttr(this)
}
object TypeAttr {
  def fromBoolTerm(t: BoolTerm): TypeAttr = t match {
    case BVar(name) => AVar(name)
    case BTrue => AUniq
    case BFalse => ANonUniq
    case BAnd(l, r) => AAnd(fromBoolTerm(l), fromBoolTerm(r))
    case BOr(l, r) => AOr(fromBoolTerm(l), fromBoolTerm(r))
    case BNot(t2) => ANot(fromBoolTerm(t2))
  }
}

case object AUniq extends TypeAttr {
  def freeTLVars = Set.empty
  def toBoolTerm = BTrue
  override def toString = Unicode.dotSymbol.toString
}
case object ANonUniq extends TypeAttr {
  def freeTLVars = Set.empty
  def toBoolTerm = BFalse
  override def toString = Unicode.crossSymbol.toString
}
case class AOr(left: TypeAttr, right: TypeAttr) extends TypeAttr {
  def freeTLVars = left.freeTLVars ++ right.freeTLVars
  def toBoolTerm = BOr(left.toBoolTerm, right.toBoolTerm)
  override def toString = "(" + left.toString + " " + Unicode.disjunctionSymbol + " " + right.toString + ")"
}
case class AAnd(left: TypeAttr, right: TypeAttr) extends TypeAttr {
  def freeTLVars = left.freeTLVars ++ right.freeTLVars
  def toBoolTerm = BAnd(left.toBoolTerm, right.toBoolTerm)
  override def toString = "(" + left.toString + " " + Unicode.conjunctionSymbol + " " + right.toString + ")"
}
case class ANot(operand: TypeAttr) extends TypeAttr {
  def freeTLVars = operand.freeTLVars
  def toBoolTerm = BNot(operand.toBoolTerm)
  override def toString = Unicode.negationSymbol + operand.toString
}
case class AVar(name: String) extends TypeAttr with KindedVar {
  def toBoolTerm = BVar(name)
}
