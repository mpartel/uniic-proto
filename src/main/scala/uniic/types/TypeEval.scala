package uniic.types
import uniic.misc.TypeError

/** Type-level evaluator.
  *
  * Currently all it does is substitute type aliases. */
object TypeEval {
  private abstract class ExpectedKind
  private case class SpecificKind(k: Kind) extends ExpectedKind
  private case object AnyKind extends ExpectedKind
  private implicit def mkSpecificKind(k: Kind) = SpecificKind(k)

  def evalTypeSchema(symTab: Map[String, Kinded], te: TypeExpr): TypeSchema = {
    kindCheckTypeSchema(evalKinded(symTab, te, KTypeSchema))
  }

  def evalType(symTab: Map[String, Kinded], te: TypeExpr): Type = {
    kindCheckType(evalKinded(symTab, te, KType))
  }

  def evalKinded(symTab: Map[String, Kinded], te: TypeExpr): Kinded = {
    evalKinded(symTab, te, AnyKind)
  }

  private def evalKinded(symTab: Map[String, Kinded], te: TypeExpr, ek: ExpectedKind): Kinded = {
    kindCheck(eval(Env(None, symTab), te, ek), ek)
  }

  private case class Env(module: Option[String], symTab: Map[String, Kinded]) {
    def apply(name: String, ek: ExpectedKind) = {
      symTab.get(name) match {
        case Some(t) => kindCheck(t, ek)
        case None => {
          ek match {
            case AnyKind => TVar(name)
            case SpecificKind(KType) => TVar(name)
            case SpecificKind(KBaseType) => TVar(name)
            case SpecificKind(KTypeAttr) => AVar(name)
            case _ => throw new TypeError("Type not found: " + name)
          }
        }
      }
    }

    def contains(name: String) = symTab.contains(name)

    def withBinding(binding: (String, Kinded)): Env = {
      Env(module, symTab + binding)
    }

    def withBindings(bindings: Seq[(String, Kinded)]): Env = {
      Env(module, symTab ++ bindings.toMap)
    }
  }

  private def eval(env: Env, te: TypeExpr, ek: ExpectedKind): Kinded = {
    te match {
      case TEUnit => TUnit(ANonUniq)
      case TEVar(name) => env(name, ek)
      case TEOr(left, right) => AOr(kindCheckAttr(eval(env, left, KTypeAttr)), kindCheckAttr(eval(env, right, KTypeAttr)))
      case TEAnd(left, right) => AAnd(kindCheckAttr(eval(env, left, KTypeAttr)), kindCheckAttr(eval(env, right, KTypeAttr)))
      case TENot(operand) => ANot(kindCheckAttr(eval(env, operand, KTypeAttr)))
      case TETuple(members) => TTuple(members.map { member => kindCheckType(eval(env, member, KType)) })
      case TEAttribution(base, attr) => Type(kindCheckBaseType(eval(env, base, KBaseType)), kindCheckAttr(eval(env, attr, KTypeAttr)))
      case TEUniq => AUniq
      case TENonUniq => ANonUniq
      case TESchema(vars, body) => {
        val tvars = schemaTypeVars(vars)
        val tbody = kindCheckType(eval(env.withBindings(tvars), body, KType))
        TypeSchema(tvars.map(_._2), tbody)
      }
      case TEFun(params, closureAttr, returnType) => {
        val tParams = params.map { case TEFunParam(ty, mode) => TFunParam(kindCheckType(eval(env, ty, KType)), mode) }
        val tClosureAttr = kindCheckAttr(eval(env, closureAttr, KTypeAttr))
        val tReturnType = kindCheckType(eval(env, returnType, KType))
        TFun(tParams, tClosureAttr, tReturnType)
      }
    }
  }

  private def schemaTypeVars(vars: Seq[(String, Kind)]): Seq[(String, KindedVar)] = {
    vars.map { case (name, kind) =>
      kind match {
        case KBaseType => (name, TVar(name))
        case KTypeAttr => (name, AVar(name))
        case KType => throw new TypeError("Type schema variables need to be base types or type attributes, not full types.")
        case KTypeSchema => throw new TypeError("Type schema variables cannot themselves be type schemas.")
      }
    }
  }

  private def kindCheck(k: Kinded, ek: ExpectedKind): Kinded = {
    ek match {
      case AnyKind => k
      case SpecificKind(KType) => kindCheckType(k)
      case SpecificKind(KBaseType) => kindCheckBaseType(k)
      case SpecificKind(KTypeAttr) => kindCheckAttr(k)
      case SpecificKind(KTypeSchema) => kindCheckTypeSchema(k)
    }
  }

  private def kindCheckTypeSchema: PartialFunction[Kinded, TypeSchema] = {
    case k: BaseType => TypeSchema(k.nonUniq) // Implicit conversion
    case k: Type => TypeSchema(k) // Implicit conversion
    case k: TypeSchema => k
    case k: Kinded => failKindCheck(k, "type schema")
  }

  private def kindCheckType: PartialFunction[Kinded, Type] = {
    case k: BaseType => k.nonUniq // Implicit conversion
    case k: Type => k
    case k: Kinded => failKindCheck(k, "type")
  }

  private def kindCheckBaseType: PartialFunction[Kinded, BaseType] = {
    case k: BaseType => k
    case k: Kinded => failKindCheck(k, "base type")
  }

  private def kindCheckAttr: PartialFunction[Kinded, TypeAttr] = {
    case k: TypeAttr => k
    case k: Kinded => failKindCheck(k, "type attribute")
  }

  private def failKindCheck[T](k: Kinded, expected: String): T = {
    throw new TypeError(k + " was a " + k.kind.name + " but " + expected + " expected")
  }
}