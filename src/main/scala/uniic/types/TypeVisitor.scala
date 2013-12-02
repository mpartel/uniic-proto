package uniic.types

trait TypeVisitor {
  def applyKinded(k: Kinded) {
    if (k.isInstanceOf[BaseType]) {
      applyBaseType(k.asInstanceOf[BaseType])
    } else if (k.isInstanceOf[TypeAttr]) {
      applyTypeAttr(k.asInstanceOf[TypeAttr])
    } else if (k.isInstanceOf[Type]) {
      applyType(k.asInstanceOf[Type])
    } else if (k.isInstanceOf[TypeSchema]) {
      applyTypeSchema(k.asInstanceOf[TypeSchema])
    } else {
      throw new IllegalArgumentException("Unknown type of kinded: " + k.getClass)
    }
  }
  def applyTypeSchema(ts: TypeSchema) {
    applyType(ts.ty)
  }
  def applyType(t: Type) {
    applyBaseType(t.baseType)
    applyTypeAttr(t.attr)
  }
  def applyBaseType(t: BaseType) {
    t match {
      case TUnit =>
      case TInt =>
      case TBool =>
      case TVar(name) =>
      case TTuple(members) => members.foreach { applyType(_) }
      case BuiltinType(_) =>
      case TFun(args, closureAttr, returnType) => {
        args.foreach {
          case TFunParam(ty, mode) => applyType(ty)
        }
        applyTypeAttr(closureAttr)
        applyType(returnType)
      }
    }
  }
  def applyTypeAttr(u: TypeAttr) {
    u match {
      case AUniq =>
      case ANonUniq =>
      case AOr(left, right) => {
        applyTypeAttr(left)
        applyTypeAttr(right)
      }
      case AAnd(left, right) => {
        applyTypeAttr(left)
        applyTypeAttr(right)
      }
      case ANot(operand) => {
        applyTypeAttr(operand)
      }
      case AVar(name) =>
    }
  }
}
