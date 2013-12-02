package uniic.grs

import uniic.types._
import uniic.misc._
import uniic.misc.graphviz.GraphvizGraph

/** Implements the type inference for De Vries' uniqueness system.
  *
  * It's more or less standard Hindley-Milner plus boolean unification
  * to deal with De Vries' uniqueness attribute equations. */
trait GrsTyping extends ExceptionContexts {
  import TypeUtils._

  import TypeUnification.Equation
  import TypeUnification.EquationSyntax
  import GrsTyping.DrawUnificationListener

  protected val typeUnification: TypeUnification = TypeUnification
  protected val drawUnification = false

  def apply(
    grs: Grs,
    symTab: Map[String, TypeSchema],
    typeSymTab: Map[String, Kinded]
  ): (Map[GrsNode, Type], Seq[Equation], Set[String]) = {
    context("generating type constraints") {
      val rigidTypeVars = {
        symTab.values.flatMap(_.freeTLVars).map(_.name).toSet ++
        grs.scopes.freeTypeLevelVars.intersect(typeSymTab.keySet)
      }

      val reservedTypeVars = rigidTypeVars ++ grs.scopes.freeTypeLevelVars

      val typeVarGen = new StreamReader(VarStream.forTVars(reservedTypeVars))
      val attrVarGen = new StreamReader(VarStream.forAVars(reservedTypeVars))

      val (rawTyping, eqs) = generateConstraints(
        symTab,
        typeSymTab,
        grs,
        typeVarGen,
        attrVarGen
      )

      (rawTyping, eqs, rigidTypeVars)
    }
  }

  def applyAndUnify(
    grs: Grs,
    symTab: Map[String, TypeSchema],
    typeSymTab: Map[String, Kinded]
  ): Map[GrsNode, Type] = {
    val (rawTyping, eqs, rigidTypeVars) = apply(grs, symTab, typeSymTab)
    unifyTyping(grs, rigidTypeVars, rawTyping, eqs)
  }

  def unifyTyping(
    grs: Grs,
    rigidTypeVars: Set[String],
    rawTyping: Map[GrsNode, Type],
    equations: Seq[Equation]
  ): Map[GrsNode, Type] = {
    val (typing, unificationResult) = tryUnifyTyping(grs, rigidTypeVars, rawTyping, equations)
    unificationResult.throwIfUnsuccessful()
    typing
  }

  def tryUnifyTyping(
    grs: Grs,
    rigidTypeVars: Set[String],
    rawTyping: Map[GrsNode, Type],
    equations: Seq[Equation]
  ): (Map[GrsNode, Type], TypeUnification.UnificationResult) = {
    val listener = {
      if (drawUnification) new DrawUnificationListener(grs, rawTyping)
      else TypeUnification.Listener.empty
    }
    val unificationResult = typeUnification.unify(equations.toList, rigidTypeVars, listener)
    verboseComment("Unification result: " + unificationResult)

    val unifierTr = unificationResult.substTr
    (rawTyping.mapValues(t => Kinded.Simplifier.applyType(unifierTr.applyType(t))), unificationResult)
  }

  private def generateConstraints(
    toplevelEnv: Map[String, TypeSchema],
    typeSymTab: Map[String, Kinded],
    grs: Grs,
    typeVarGen: StreamReader[TVar],
    attrVarGen: StreamReader[AVar]
  ): (Map[GrsNode, Type], Seq[Equation]) = {
    def freshTypeWithAttr() = typeVarGen.take().withAttr(attrVarGen.take())

    // Initially all nodes are typed as a fresh variable, except those with an assigned type.
    // We also assign trivial types here, to produce less equations and thus ease debugging.
    val typing: Map[GrsNode, Type] = {
      grs.allNodes.map { node =>
        val ty0: Type = node.value match {
          case GrsUnit => TUnit.nonUniq
          case GrsBool(_) => TBool.nonUniq
          case GrsInt(_) => TInt.nonUniq
          case GrsBuiltinFunction(name, _, _) => {
            toplevelEnv(name).instantiateWithFreshVars(typeVarGen, attrVarGen)
          }
          case GrsBuiltinValue(_, Some(ty)) => ty.withAttr(attrVarGen.take())
          case GrsBuiltinValue(value, None) => {
            throw new TypeError("Builtin value with no type information: " + value)
          }
          case GrsVar(v) => {
            grs.scopes.declarationSiteOf(node.downcast[GrsVar]) match {
              case Some(_) => freshTypeWithAttr()
              case None => {
                // Free variable. Normally these are GrsExternal, but some tests use GrsVar.
                toplevelEnv(v).instantiateWithFreshVars(typeVarGen, attrVarGen)
              }
            }
          }
          case GrsExternal(v) => {
            toplevelEnv(v).instantiateWithFreshVars(typeVarGen, attrVarGen)
          }
          case GrsCase => TInternal.nonUniq
          case GrsParamList(_) => TInternal.nonUniq
          case GrsClosure(_) => TInternal.nonUniq
          case _ => freshTypeWithAttr()
        }
        val ty = grs.refCounts.forNode(node) match {
          case UnsharedRef => ty0
          case SharedRef => ty0.baseType.nonUniq
        }
        node -> ty
      }.toMap
    }

    def getConstraints(node: GrsNode): Seq[Equation] = {
      val initialTy = typing(node)
      node.value match {
        // No equations needed for things with initial typing
        case GrsUnit => Seq()
        case _: GrsBool => Seq()
        case _: GrsInt => Seq()
        case _: GrsVar => Seq()
        case _: GrsBuiltinFunction => Seq()
        case _: GrsBuiltinValue => Seq()
        case _: GrsExternal => Seq()
        case GrsApply(_) => {
          val leftTy = typing(node.children(0))
          val rightTys = node.children.tail.map(e => typing(e).param)
          Seq(leftTy.baseType =:= TFun(rightTys, leftTy.attr, initialTy))
        }
        case GrsLambda(paramTypeAnnotations) => {
          if (!node.children(0).value.isInstanceOf[GrsParamList]) {
            throw new TypeError("GrsLambda's 1st child must be a GrsParamList")
          }
          if (node.children(0).children.length != paramTypeAnnotations.length) {
            throw new TypeError("GrsLambda's param count is wrong")
          }
          if (!node.children(1).value.isInstanceOf[GrsClosure]) {
            throw new TypeError("GrsLambda's 2nd child must be a GrsClosure")
          }

          val paramNodes = node.children(0).children.map(_.downcast[GrsVar])
          val paramTypes = node.children(0).children.map(typing).map(_.param)
          val paramEqs = paramTypeAnnotations.zipWithIndex.flatMap {
            case (MissingTypeAnnotation, _) => None
            case (SimpleTypeAnnotation(ty), i) => Some(typing(paramNodes(i)) =:= ty)
            case (TypeExprTypeAnnotation(te), i) => {
              val evaledType = TypeEval.evalType(typeSymTab, te)
              Some(typing(paramNodes(i)) =:= evaledType)
            }
          }

          val bodyNode = node.children(2)
          val returnType = typing(bodyNode)

          val closureNodes = node.children(1).children

          val closureAttrs = closureNodes.map(vn => typing(vn).attr)
          val closureAttr = {
            if (closureAttrs.isEmpty) ANonUniq
            else closureAttrs.reduceLeft(AOr(_, _))
          }
          paramEqs :+ (initialTy.baseType =:= TFun(paramTypes, closureAttr, returnType))
        }
        case GrsIfThenElse => {
          val condTy = typing(node.children(0))
          val thenTy = typing(node.children(1))
          val elseTy = typing(node.children(2))
          Seq(condTy.baseType =:= TBool, initialTy =:= thenTy, initialTy =:= elseTy)
        }
        case GrsTuple(n) => {
          val childAttrs = node.children.map(e => typing(e).attr)
          val attr = childAttrs.foldLeft(attrVarGen.take(): TypeAttr)(AOr(_, _))
          Seq(
            initialTy.baseType =:= TTuple(node.children.map(typing)),
            initialTy.attr =:= attr
          )
        }
        case GrsMatch(numCases) => {
          val headTy = typing(node.children(0))
          val patternTys = node.children.tail.map(c => typing(c.children(0)))
          val caseTys = node.children.tail.map(c => typing(c.children(1)))
          patternTys.map(headTy =:= _) ++ caseTys.map(initialTy =:= _)
        }
        case GrsCase => Seq() // Handled with GrsMatch
        case GrsParamList(_) => Seq() // Handled with GrsLambda
        case GrsClosure(_) => Seq() // Hanled with GrsLambda
        case GrsUnused => Seq() // Needs no constraints
        case GrsRoot => Seq(initialTy =:= typing(node.children(0)))
        case GrsThunk => {
          throw new CompilerException("GrsThunk cannot be typed")
        }
        case _: GrsModule => {
          throw new CompilerException("GrsModule cannot be typed")
        }
      }
    }

    val constraints: Seq[Equation] = typing.keys.flatMap(getConstraints).toSeq

    verboseComment("Typing: " + typing.mkString("\n"))
    verboseComment("Constraints: " + constraints.mkString("\n"))
    (typing, constraints)
  }

  private def throwBorrowingSeen[T](): T = {
    throw new CompilerException("Borrowing should have been eliminated already")
  }

  def typingToGraphviz(grs: Grs, typing: Map[GrsNode, Type], equations: Seq[Equation] = Seq.empty): GraphvizGraph = {
    val gg = grs.toGraphviz

    for ((node, ty) <- typing) {
      val ggNode = gg.findNode(node).get
      ggNode.attributes("label") = "\\l" + ggNode.attributes("label") + "\\l" + ty.toString
    }

    if (!equations.isEmpty) {
      val label = "\\lEquations:\\n" + equations.mkString("\\n") + "\\n"
      gg.addNode(label, "shape" -> "rectangle")
    }

    gg
  }
}

object GrsTyping extends GrsTyping with HasDebugVersion[GrsTyping] {
  val withDebug = new GrsTyping with DebugContexts {
    override protected val typeUnification = new TypeUnification with DebugContexts
  }
  val withUnificationDrawings = new GrsTyping with DebugContexts {
    override protected val typeUnification = new TypeUnification with DebugContexts
    override protected val drawUnification = true
  }

  class DrawUnificationListener(grs: Grs, rawTyping: Map[GrsNode, Type]) extends TypeUnification.Listener {
    import TypeUnification.Equation

    private var prev = rawTyping

    override def onLoop(iter: Int, substs: Seq[KindedSubst], remainingEqs: List[Equation]) {
      val halfUnifiedTyping = rawTyping.mapValues { t =>
        substs.foldLeft(t) { case (t, s) => s.applyType(t) }
      }
      val gg = typingToGraphviz(grs, halfUnifiedTyping)

      val changedNodes = rawTyping.keys.filter(k => halfUnifiedTyping(k) != prev(k))
      for (c <- changedNodes; n <- gg.findNode(c)) {
        n.attributes("color") = "red"
      }
      prev = halfUnifiedTyping

      val remainingEqsLabel = remainingEqs.take(5).mkString("\\lNext:\\l", "\\l", "\\l...")
      gg.addNode(remainingEqsLabel, "shape" -> "rectangle")

      gg.setName(s"unification_$iter")
      gg.writeImageToDebugDir()
    }
  }
}
