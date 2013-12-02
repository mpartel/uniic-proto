package uniic.fun
import uniic.parsers.Lexer
import uniic.misc.Unicode
import uniic.types._

object FunExprToIndentedString extends (FunExpr => String) {
  val indentStep = "  "

  def apply(e: FunExpr) = {
    try {
      apply(e, 0)
    } catch {
      case e: Exception => "<FunExpr's toString failed: " + e.getClass.getName + ">"
    }
  }

  private def apply(e: FunExpr, indLvl: Int): String = {
    lazy val ind = indentStep * indLvl
    lazy val indPlus1 = indentStep * (indLvl + 1)
    lazy val indPlus2 = indentStep * (indLvl + 2)
    e match {
      case FunUnit => "()"
      case FunInt(v) => v.toString
      case FunBool(v) => v.toString
      case FunVar(name) => name
      case FunLambda(params, body) => {
        val paramStrs = params.map {
          case (name, SimpleTypeAnnotation(ty)) => name + ": " + ty
          case (name, TypeExprTypeAnnotation(te)) => name + ": " + te
          case (name, MissingTypeAnnotation) => name
        }
        "(" +
          Unicode.lambdaSymbol + " " + paramStrs.mkString(", ") + ".\n" +
          indPlus1 + apply(body, indLvl + 1) +
        ")"
      }
      case FunApply(fun, args) => {
        fun match {
          case FunVar(op) if args.size == 2 && Lexer.isIdentOperator(op) => {
            val mid = if (op == "." || op == "?.") op else " " + op + " "
            apply(args(0), indLvl) + mid + apply(args(1), indLvl)
          }
          case _ => {
            apply(fun, indLvl) + "(" + args.map(apply(_, indLvl)).mkString(", ") + ")"
          }
        }
      }
      case FunLet(bindings, body) => {
        val bindingStrs = bindings.map {
          case (v, b) => {
            val bStr = apply(b, indLvl + 1)
            if (bStr.contains('\n')) {
              v + " =\n" + indPlus1 + bStr
            } else {
              v + " = " + bStr
            }
          }
        }
        bindingStrs.mkString("let ", "\n" + ind + "and ", "\n") +
          ind + "in\n" +
          indPlus1 + apply(body, indLvl + 1)
      }
      case FunIfThenElse(cond, thenExpr, elseExpr) => {
        "if " + apply(cond, indLvl) + "\n" +
          indPlus1 + "then\n" +
          indPlus2 + apply(thenExpr, indLvl + 2) + "\n" +
          indPlus1 + "else\n" +
          indPlus2 + apply(elseExpr, indLvl + 2)
      }
      case FunTupleExpr(members) => "(" + members.map(apply(_, indLvl + 1)).mkString(",") + ")"
      case f: FunBuiltinFunction => f.name
      case v: FunBuiltinValue => v.toString
      case FunMatch(head, clauses) => {
        val clauseStrs = clauses.map {
          case FunCase(pattern, body) => {
            indPlus1 + "case " + pattern.toString + " =>\n" + indPlus2 + apply(body, indLvl + 2)
          }
        }
        apply(head, indLvl) + " match {\n" + clauseStrs.mkString("\n") + "\n" + ind + "}"
      }
    }
  }
}

object FunExprToShortString extends (FunExpr => String) {
  def apply(expr: FunExpr) = expr match {
    case FunUnit => "Unit"
    case FunBool(v) => v.toString
    case FunInt(v) => v.toString
    case FunVar(v) => v.toString
    case FunTupleExpr(members) => "Tuple"
    case FunLambda(params, body) => Unicode.lambdaSymbol + params.map(_._1).mkString(",")
    case FunBuiltinFunction(name, _, _, _) => "Builtin(" + name + ")"
    case FunBuiltinValue(value, _) => value.toString
    case FunApply(_, _) => "Apply"
    case FunLet(bindings, body) => "Let(" + bindings.map(_._1).mkString(",") + ")"
    case FunIfThenElse(_, _, _) => "If"
    case FunMatch(_, _) => "Match"
  }
}