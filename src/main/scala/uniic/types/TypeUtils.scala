package uniic.types
import uniic.misc.TypeError
import uniic.misc.CompilerException

object TypeUtils {
  /** Finds all TVars and AVars in this type or throws TypeError if there is a TVar and an AVar with the same name. */
  def findVars(ty: Type): Set[KindedVar] = {
    def conflict(name: String) = new TypeError("Type contains " + name + " as both a type variable and a type attribute")

    var result = Set.empty[KindedVar]
    new TypeVisitor {
      override def applyBaseType(t: BaseType) {
        t match {
          case TVar(name) => {
            if (result.contains(AVar(name))) {
              throw conflict(name)
            }
            result += TVar(name)
          }
          case _ => super.applyBaseType(t)
        }
      }
      override def applyTypeAttr(a: TypeAttr) {
        a match {
          case AVar(name) => {
            if (result.contains(TVar(name))) {
              throw conflict(name)
            }
            result += AVar(name)
          }
          case _ => super.applyTypeAttr(a)
        }
      }
    }.applyType(ty)
    result
  }
}