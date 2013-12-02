package uniic.module

import uniic.misc._
import uniic.imp._
import uniic.flow._
import uniic.fun._
import uniic.grs._
import uniic.types._
import uniic.misc.graphviz.GraphvizGraph
import uniic.fun.analysis.FunTree

/** A state that a function goes through on its way from imperative code to a typed GRS. */
sealed trait Stage extends ExceptionContexts with DebugContexts {
  val ctx: Stage.Context

  def name = this.getClass.getSimpleName
  def next: Stage
  def isFinal = false

  def isDebugged = ctx.debugSettings.debugStage(this)
  override def logLevel = if (isDebugged) Contexts.DefaultLevel else Contexts.OffLevel

  def to[P <: Stage : Manifest]: P = {
    val m = manifest[P]
    var p = this
    while (!p.isFinal && !m.runtimeClass.isInstance(p)) {
      p = p.next
    }
    if (m.runtimeClass.isInstance(p)) {
      p.asInstanceOf[P]
    } else {
      throw new CompilerException(
        s"Cannot get from ${this.name} to ${m.runtimeClass.getSimpleName}"
      )
    }
  }

  protected def debugGraphviz[T <: { def toGraphviz: GraphvizGraph }](name: String, obj: => T): T = {
    if (this.isDebugged) {
      debugGraphviz(name, obj.toGraphviz)
    }
    obj
  }

  protected def debugGraphviz(name: String, gg: GraphvizGraph) {
    if (this.isDebugged) {
      gg.setName(ctx.debugSettings.graphvizFilePrefix + name).writeImageToDebugDir()
    }
  }
}

object Stage {
  case class SymTabEntry(
    value: GrsValue,
    borrowingType: TypeSchema,
    nonborrowingType: TypeSchema
  )

  case class Context(
    symTab: Map[String, SymTabEntry],
    typeSymTab: Map[String, Kinded],
    debugSettings: DebugSettings = DebugSettings.noDebug
  )

  case class DebugSettings(
    debugStage: Stage => Boolean,
    graphvizFilePrefix: String = ""
  )

  object DebugSettings {
    val noDebug = DebugSettings(_ => false)

    val fullDebug = DebugSettings(_ => true)

    def choose(wantDebug: Boolean): DebugSettings = {
      if (wantDebug) fullDebug else noDebug
    }
  }
}

case class ImpStage(ctx: Stage.Context, impToplevel: ImpToplevel) extends Stage {
  lazy val next = context("transforming imperative code to flowgraph") {
    impToplevel match {
      case ImpFunDef(params, wantedReturnType, body) => {
        val paramsWithEvaluatedTypes = params.map { case (name, TEFunParam(te, node)) =>
          val ty = TypeEval.evalType(ctx.typeSymTab, te)
          name -> TFunParam(ty, node)
        }

        val wantedReturnTypeEvaluated = wantedReturnType.map(t => TypeEval.evalType(ctx.typeSymTab, t))

        val flowStmts = ImpToFlow(body)
        val flowGraph = debugGraphviz("flow", FlowGraph.fromStatements(flowStmts))
        FlowStage(ctx, FlowFunDef(paramsWithEvaluatedTypes, wantedReturnTypeEvaluated, flowGraph))
      }
    }
  }
}

case class FlowStage(ctx: Stage.Context, flowToplevel: FlowToplevel) extends Stage {
  lazy val next = context("transforming flowgraph to SSA") {
    flowToplevel match {
      case funDef@FlowFunDef(params, wantedReturnType, flowGraph) => {
        FlowUseBeforeDefinitionAnalysis.check(flowGraph, params.map(p => FlowVar(p._1, 0)).toSet)

        val ssaGraph = FlowToSSA(flowGraph, params.map(_._1).toSet)
        debugGraphviz("ssa", ssaGraph.toGraphviz)
        InitialSSAStage(ctx, FlowFunDef(params, wantedReturnType, ssaGraph))
      }
    }
  }
}

case class InitialSSAStage(
  ctx: Stage.Context,
  flowToplevel: FlowToplevel
) extends Stage with FlowStageUtils {
  lazy val next = context("type-checking SSA") {
    flowToplevel match {
      case funDef@FlowFunDef(params, wantedReturnType, flowGraph) => {
        val ssaTypingResult = doSSATyping(params, wantedReturnType, flowGraph, useBorrowingSymTab = true)

        //debugGraphviz("ssaty", SSATyping.typingToGraphviz(flowGraph, ssaTypingResult.typing))

        val newFunDef = funDef.copy(returnType = Some(ssaTypingResult.returnType))
        BorrowingTypedSSAStage(ctx, newFunDef, ssaTypingResult)
      }
    }
  }
}

case class BorrowingTypedSSAStage(
  ctx: Stage.Context,
  flowToplevel: FlowToplevel,
  borrowingSsaTypingResult: SSATyping.Result
) extends Stage with FlowStageUtils {
  lazy val next = context("removing borrowing") {
    flowToplevel match {
      case funDef@FlowFunDef(params, Some(wantedReturnType), flowGraph) => {
        val paramsAndModes = params.map { case (n, TFunParam(_, m)) => (n, m) }
        val newFlowGraph = removeBorrowingFromFlowgraph(flowGraph, paramsAndModes)
        debugGraphviz("flow_nb", newFlowGraph)

        val existingAVars = params.flatMap(_._2.ty.freeTLVars).toSet ++ wantedReturnType.freeTLVars
        val freshAVars = new StreamReader(VarStream.forAVars(existingAVars.map(_.name)))
        val borrowingFunTy = TFun(params.map(_._2), ANonUniq, wantedReturnType)
        val nonborrowinFunTy = new TypeBorrowingRemoval(freshAVars).applyBaseType(borrowingFunTy)
        val TFun(newParamTypes, _, newReturnTy) = nonborrowinFunTy

        val newParams = params.map(_._1).zip(newParamTypes)
        val newFunDef = FlowFunDef(newParams, Some(newReturnTy), newFlowGraph)

        val nonborrowingSsaTypingResult = doSSATyping(newParams, Some(newReturnTy), newFlowGraph, useBorrowingSymTab = false)

        //debugGraphviz("ssaty_nb", SSATyping.typingToGraphviz(flowGraph, ssaTypingResult.typing))

        NonborrowingTypedSSAStage(ctx, newFunDef, borrowingSsaTypingResult, nonborrowingSsaTypingResult)
      }
      case FlowFunDef(_, None, _) => throw new CompilerException("Typed SSA stage")
    }
  }

  private def removeBorrowingFromFlowgraph(borrowingFlowGraph: FlowGraph, paramsAndModes: Seq[(String, ParamMode)]): FlowGraph = {
    // Borrowing removals don't preserve single assignment, so we need to repeat the SSA transformation here.
    val g1 = FlowRemoveBorrowingFromCallSites(borrowingFlowGraph)
    val g2 = FlowRemoveBorrowingFromReturnSites(g1, paramsAndModes)
    FlowToSSA(g2, paramsAndModes.map(_._1).toSet)
  }
}

case class NonborrowingTypedSSAStage(
  ctx: Stage.Context,
  flowToplevel: FlowToplevel,
  borrowingSsaTypingResult: SSATyping.Result,
  nonborrowingSsaTypingResult: SSATyping.Result
) extends Stage {
  lazy val next = context("transforming SSA to functional") {
    flowToplevel match {
      case funDef@FlowFunDef(params, expectedReturnType, flowGraph) => {
        val body = SSAToFun(flowGraph)
        val lambdaParams = params.map {
          case (name, TFunParam(ty, MNone)) => name -> TypeAnnotation(ty)
          case (_, TFunParam(_, MBorrowed)) => throw new IllegalArgumentException("Borrowed param annotation made it to SSA stage")
        }
        val lambda = FunLambda(lambdaParams, body)
        val tree = debugGraphviz("fun", new FunTree(lambda))
        FunStage(ctx, tree, Some(nonborrowingSsaTypingResult))
      }
    }
  }
}

case class FunStage(
  ctx: Stage.Context,
  funExpr: FunTree,
  ssaTypingResult: Option[SSATyping.Result]
) extends Stage {
  lazy val next = context("transforming functional code to a GRS") {
    val grs = debugGraphviz("grs", FunToGrs(funExpr, ctx.symTab.mapValues(_.value)))
    UntypedGrsStage(ctx, grs, ssaTypingResult)
  }
}

case class UntypedGrsStage(
  ctx: Stage.Context,
  grs: Grs,
  ssaTypingResult: Option[SSATyping.Result]
) extends Stage {
  lazy val next = context("type-checking GRS") {
    val grsTyping = GrsTyping.withDebug(this.isDebugged)
    //val grsTyping = GrsTyping.withUnificationDrawings // TODO: also draw eqs generated during last step

    debugGraphviz("grsref", grs.refCounts.toGraphviz)

    val (rawTyping, equations, rigidTypeVars) = GrsTyping.apply(
      grs,
      ctx.symTab.mapValues(_.nonborrowingType),
      ctx.typeSymTab
    )
    debugGraphviz("grsty_raw", grsTyping.typingToGraphviz(grs, rawTyping, equations))

    val typing = grsTyping.unifyTyping(grs, rigidTypeVars, rawTyping, equations)
    debugGraphviz("grsty", grsTyping.typingToGraphviz(grs, typing))

    val rootType = typing(grs.rootNode).generalize(Set.empty)

    comment(s"GRS type: $rootType")

    ssaTypingResult.foreach { ssatr =>
      if (!rootType.isEquivalentTo(ssatr.functionType)) {
        throw new CompilerException(
          "SSA and GRS types did not match:\n" +
          s"  SSA type = ${ssatr.functionType}\n" +
          s"  GRS type = $rootType\n"
        )
      }
    }

    TypedGrsStage(ctx, grs, typing, rootType)
  }
}

case class TypedGrsStage(
  ctx: Stage.Context,
  grs: Grs,
  typing: Map[GrsNode, Type],
  rootType: TypeSchema
) extends Stage {
  def next = this
  override def isFinal = true
}
