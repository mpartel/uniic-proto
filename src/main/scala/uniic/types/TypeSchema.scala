package uniic.types
import uniic.misc._

case class TypeSchema(
  vars: Seq[KindedVar],
  ty: Type
) extends Kinded {
  override def kind = KTypeSchema

  override def freeTLVars = rigidVars

  override def toString = {
    val varPart = {
      if (vars.isEmpty) {
        Unicode.forallSymbol + "()"
      } else {
        Unicode.forallSymbol + vars.mkString(" ")
      }
    }
    varPart + "." + ty
  }

  def subst(toReplace: TVar, replacement: BaseType) = TVarSubst(toReplace, replacement).applyTypeSchema(this)
  def subst(toReplace: AVar, replacement: TypeAttr) = AVarSubst(toReplace, replacement).applyTypeSchema(this)
  def subst(toReplace: Type, replacement: Type) = TypeSubst(toReplace, replacement).applyTypeSchema(this)

  lazy val rigidVars: Set[KindedVar] = ty.freeTLVars -- vars

  def instantiateWithFreshVars(existingVars: Set[String]): Type = {
    val (tvars, avars) = freeVarStreams(existingVars)
    instantiateWithFreshVars(tvars, avars)
  }

  def instantiateWithFreshVars(implicit tvars: StreamReader[TVar], avars: StreamReader[AVar]): Type = {
    val inst = getInstantiationToFreshVars(tvars, avars)
    inst.applyType(this.ty)
  }

  def getInstantiationToFreshVars(existingVars: Set[String]): VarToVarSubstSeq = {
    val (tvars, avars) = freeVarStreams(existingVars)
    getInstantiationToFreshVars(tvars, avars)
  }

  def getInstantiationToFreshVars(tvars: StreamReader[TVar], avars: StreamReader[AVar]): VarToVarSubstSeq = {
    VarToVarSubstSeq(this.vars.map {
      case v: TVar => VarToVarSubst(v, tvars.take())
      case v: AVar => VarToVarSubst(v, avars.take())
    })
  }

  private[this] def freeVarStreams(existingVars: Set[String]): (StreamReader[TVar], StreamReader[AVar]) = {
    val reserved = existingVars ++ this.vars.map(_.name)
    val tvars = new StreamReader(VarStream.forTVars(reserved))
    val avars = new StreamReader(VarStream.forAVars(reserved))
    (tvars, avars)
  }

  /** Finds a substitution of `this.vars` such that `this.ty` becomes `that.ty`.
    *
    * @throw TypeError no such substitution could be found (by unification). */
  def getInstantiationTo(that: TypeSchema): TypeTransformation = {
    // We subst this.vars with temporary names in case this.vars and that.vars overlap.

    val existingVars = this.vars.toSet ++ that.vars.toSet ++ this.ty.freeTLVars ++ that.ty.freeTLVars
    val toTempVars = this.getInstantiationToFreshVars(existingVars.map(_.name))
    val fromTempVars = toTempVars.reverse

    val thisWithTempVars = toTempVars.applyType(this.ty)
    val unificationResult = TypeUnification.unify(
      List(TypeUnification.Equation(thisWithTempVars, that.ty)),
      existingVars.map(_.name)
    )

    unificationResult.throwIfUnsuccessful()

    toTempVars ++ TypeTransformationSeq(unificationResult.subst) ++ fromTempVars
  }

  /** Tells whether there is a substitition of `that.vars` that instantiates `that.ty` into `this.ty`. */
  def canBeInstantiatedFrom(that: TypeSchema): Boolean = {
    try {
      that.getInstantiationTo(this)
      true
    } catch {
      case _: TypeError => false
    }
  }

  def <=(that: TypeSchema) = this.canBeInstantiatedFrom(that)
  def >=(that: TypeSchema) = that.canBeInstantiatedFrom(this)

  def isEquivalentTo(that: TypeSchema): Boolean = {
    this.canBeInstantiatedFrom(that) && that.canBeInstantiatedFrom(this)
  }

  /** Drops unused variables and simplifies the attributes in `ty`. */
  def simplify: TypeSchema = {
    TypeSchema(usedVars, Kinded.Simplifier.applyType(ty))
  }

  def dropUnusedVarsVars: TypeSchema = {
    TypeSchema(usedVars, ty)
  }

  def usedVars: Seq[KindedVar] = {
    val freeVars = ty.freeTLVars
    vars.filter(freeVars.contains(_))
  }
}

object TypeSchema {
  def apply(ty: Type) = new TypeSchema(Seq.empty, ty)
}
