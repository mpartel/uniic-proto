package uniic.types

import uniic.misc.VarStream
import uniic.misc.StreamReader

class TypeBorrowingRemoval(val freshAVars: StreamReader[AVar]) extends TypeTransformation {
  override def applyBaseType(bt: BaseType): BaseType = {
    bt match {
      case fun: TFun => {
        val newParams = fun.params.map {
          case TFunParam(t, m) => TFunParam(applyType(t), MNone)
        }

        val newReturn = {
          if (fun.borrowedParams.isEmpty) {
            applyType(fun.returnType)
          } else {
            TTuple(applyType(fun.returnType) +: fun.borrowedParamTypes).uniq
          }
        }

        TFun(newParams, fun.closureAttr, newReturn)
      }
      case _ => super.applyBaseType(bt)
    }
  }
}

object TypeBorrowingRemoval {
  def apply(k: Kinded, freshAVars: StreamReader[AVar]): Kinded = {
    k match {
      case ts: TypeSchema => apply(ts, freshAVars)
      case k: Rank1Kinded => {
        val tbr = new TypeBorrowingRemoval(freshAVars)
        tbr.applyKinded(k)
      }
    }
  }

  def apply(ts: TypeSchema, freshAVars: StreamReader[AVar]): TypeSchema = {
    val tbr = new TypeBorrowingRemoval(freshAVars)
    tbr.applyType(ts.ty).generalize(ts.rigidVars.map(_.name))
  }

  def apply(ts: TypeSchema): TypeSchema = {
    val freshAVars = new StreamReader(VarStream.forAVars(ts.ty.freeAVars.map(_.name)))
    this(ts, freshAVars)
  }
}
