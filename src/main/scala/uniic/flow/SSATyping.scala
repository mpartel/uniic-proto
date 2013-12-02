package uniic.flow
import uniic.types._
import uniic.misc._
import uniic.fun._
import uniic.fun.analysis._
import uniic.imp._
import scala.collection.mutable
import uniic.misc.graphviz.GraphvizGraph

/** Type-checks an SSA flow graph. */
class SSATyping extends ExceptionContexts {
  import SSATyping._
  import TypeUnification.Equation
  import TypeUnification.EquationSyntax

  protected val typeUnification: TypeUnification = TypeUnification

  private case class SymTab(
    externalSymbols: Map[FlowVar, TypeSchema],
    localSymbols: Map[FlowVar, Type]
  ) {
    def apply(v: FlowVar)(implicit tvr: StreamReader[TVar], avr: StreamReader[AVar]): Type = {
      localSymbols.get(v) match {
        case Some(t) => t
        case None => {
          val ts = externalSymbols.getOrElse(v, fail(s"Undefined variable: $v"))
          ts.instantiateWithFreshVars
        }
      }
    }

    lazy val keySet = externalSymbols.keySet ++ localSymbols.keySet

    def withLocals(moreLocals: Map[FlowVar, Type]) = this.copy(localSymbols = localSymbols ++ moreLocals)
  }

  /** Type-checks the given flow graph. */
  def apply(
    flowGraph: FlowGraph,
    externalSymbols: Map[String, TypeSchema],
    params: Seq[(String, Type, ParamMode)],
    wantedReturnType: Option[Type],
    typeSymTab: Map[String, Kinded]
  ): Result = {
    val Constraints(rawTyping, eqs, rigids, returnTy) = generateConstraints(flowGraph, externalSymbols, params, wantedReturnType, typeSymTab)
    verboseComment("Raw typing:\n" + CollectionUtil.pairsToPrettyString(rawTyping))

    val unifier = typeUnification.unify(eqs.toList, rigids)
    val tr = unifier.substTrIfSuccessful
    verboseComment("Unifier: " + tr)

    lazy val unifiedTyping = rawTyping.mapValues(t => tr.applyType(t))
    verboseComment("Typing:\n" + CollectionUtil.pairsToPrettyString(unifiedTyping))

    val unifiedParams = params.map { case (n, t, m) => (n, tr.applyType(t), m) }
    val unifiedReturnTy = tr.applyType(returnTy)

    Result(flowGraph, unifiedParams, unifiedReturnTy)
  }

  /**
   * Generates an initial ununified typing, the set of unification equations
   * and the set of type variables that should be rigid during unification.
   */
  def generateConstraints(
    flowGraph: FlowGraph,
    externalSymbols: Map[String, TypeSchema],
    params: Seq[(String, Type, ParamMode)],
    wantedReturnType: Option[Type],
    typeSymTab: Map[String, Kinded]
  ): Constraints = {
    val paramRigidVars = wantedReturnType match {
      case Some(r) => r.freeTLVars.map(_.name)
      case None => Seq.empty
    }
    val paramSymbols: Map[String, Type] = params.map { case (n, t, _) => (n, t) }.toMap

    val rigidTypeVars: Set[String] = (
      paramSymbols.flatMap(_._2.freeTLVars).toSet ++ wantedReturnType.toSeq.flatMap(_.freeTLVars)
    ).map(_.name)
    implicit val typeVarGen = new StreamReader(VarStream.forTVars(rigidTypeVars))
    implicit val attrVarGen = new StreamReader(VarStream.forAVars(rigidTypeVars))

    val returnTy = wantedReturnType match {
      case Some(r) => r
      case None => typeVarGen.take().withAttr(attrVarGen.take())
    }

    val extSymTab = SymTab(
      externalSymbols = keysToFlowVar(externalSymbols),
      localSymbols = keysToFlowVar(paramSymbols)
    )

    val symTab = initialTypeAssignments(flowGraph, extSymTab)

    val eqs = traverseFlowgraph(flowGraph, symTab, returnTy)

    Constraints(symTab.localSymbols, eqs, rigidTypeVars, returnTy)
  }

  private def keysToFlowVar[A](map: Map[String, A]): Map[FlowVar, A] = {
    map.map { case (k, v) => FlowVar(k, 0) -> v }
  }

  private def initialTypeAssignments(
    flowGraph: FlowGraph,
    extSymTab: SymTab
  )(
    implicit
    tvr: StreamReader[TVar],
    avr: StreamReader[AVar]
  ): SymTab = {
    val allLocalVars: Set[FlowVar] = flowGraph.allVars -- (extSymTab.externalSymbols.keySet -- extSymTab.localSymbols.keySet)

    val locals: Map[FlowVar, Type] = allLocalVars.map { v =>
      import FlowReadCountAnalysis._

      val count: ReadCount = flowGraph.getAssigningStmtAddrs(v) match {
        case Seq() => flowGraph.readCountAnalysis.result.blockInputs(flowGraph.start)(v)
        case Seq(writeAddr) => flowGraph.readCountAnalysis.after(writeAddr)(v)
        case _ => throw new CompilerException(s"This is not an SSA: $v was assigned more than once")
      }

      val t = {
        if (count > ReadOnce) {
          tvr.take().nonUniq
        } else {
          tvr.take().withAttr(avr.take())
        }
      }

      v -> t
    }.toMap

    val unusedLocals: Map[FlowVar, Type] = (for {
      stmt <- flowGraph.allStmts
      v <- stmt.resultVars
      if !locals.contains(v)
    } yield {
      v -> tvr.take().withAttr(avr.take())
    }).toMap

    extSymTab.withLocals(locals ++ unusedLocals)
  }

  private def traverseFlowgraph(
    flowGraph: FlowGraph,
    symTab: SymTab,
    functionReturnType: Type
  )(
    implicit
    typeVarGen: StreamReader[TVar],
    attrVarGen: StreamReader[AVar]
  ): Seq[Equation] = {
    flowGraph.blocksInOrder.map(_._2).flatMap(traverseBlock(symTab, _, functionReturnType))
  }

  private def traverseBlock(
    symTab: SymTab,
    block: FlowBlock,
    expectedReturnType: Type
  )(
    implicit
    typeVarGen: StreamReader[TVar],
    attrVarGen: StreamReader[AVar]
  ): Seq[Equation] = {
    block.allStmts.flatMap { stmt =>
      val eqs = traverseStmt(symTab, stmt, expectedReturnType)
      verboseComment(s"$stmt generated eqs: $eqs")
      eqs
    }
  }

  private def traverseStmt(
    symTab: SymTab,
    stmt: FlowStmt,
    expectedReturnType: Type
  )(
    implicit
    typeVarGen: StreamReader[TVar],
    attrVarGen: StreamReader[AVar]
  ): Seq[Equation] = {
    stmt match {
      case FlowLabel(_) => Seq.empty
      case FlowPhiStmt(v, inputs) => {
        val tv = symTab(v)
        inputs.map(tv =:= symTab(_))
      }
      case FlowSetConst(v, value) => {
        Seq(symTab(v) =:= constType(value))
      }
      case FlowSet(v, from) => {
        Seq(symTab(v) =:= symTab(from))
      }
      case FlowBorrowingRemoval(v, from) => {
        Seq(symTab(v) =:= symTab(from))
      }
      case FlowCall(v, f, args) => {
        val argTypes = args.map(a => TFunParam(symTab(a.in), a.mode))
        val argEqs = args.collect {
          case FlowBorrowedArg(in, out) => symTab(in) =:= symTab(out)
        }
        val funAttr = attrVarGen.take()
        val returnTy = symTab(v)
        argEqs :+ (symTab(f) =:= TFun(argTypes, funAttr, returnTy).withAttr(funAttr))
      }
      case FlowMakeTuple(v, args) => {
        val argTypes = args.map(symTab(_))
        val freeAttr = attrVarGen.take()
        val tupleAttr = argTypes.map(_.attr).foldLeft(freeAttr: TypeAttr)(AOr(_, _))
        Seq(symTab(v) =:= TTuple(argTypes).withAttr(tupleAttr))
      }
      case FlowSplitTuple(vars, tupleVar) => {
        val varTypes = vars.map(symTab(_))
        val Type(t, _) = symTab(tupleVar)
        Seq(t =:= TTuple(varTypes))
      }
      case FlowGoto(_) => Seq.empty
      case FlowGotoCond(v, _, _) => Seq(symTab(v).baseType =:= TBool)
      case FlowReturn(v) => Seq(symTab(v) =:= expectedReturnType)
    }
  }

  private def constType(v: ImpConst): Type = {
    v match {
      case ImpUnit => TUnit.nonUniq
      case _: ImpInt => TInt.nonUniq
      case _: ImpBool => TBool.nonUniq
    }
  }

  private def fail[T](msg: String): T = {
    throw new TypeError(msg)
  }
}



object SSATyping extends SSATyping with HasDebugVersion[SSATyping] {
  val withDebug = new SSATyping with DebugContexts {
    override protected val typeUnification = TypeUnification.withDebug
    override def logLevel = Contexts.VerboseLevel
  }

  case class Constraints(
    rawTyping: Map[FlowVar, Type],
    equations: Seq[TypeUnification.Equation],
    rigidTypeVars: Set[String],
    returnType: Type
  )

  case class Result(
    flowGraph: FlowGraph,
    params: Seq[(String, Type, ParamMode)],
    returnType: Type
  ) {
    val functionType: TypeSchema = {
      val tFunParams = params.map { case (_, ty, m) => TFunParam(ty, m) }
      val usedVars = tFunParams.flatMap(_.ty.freeAVars.map(_.name)) ++ returnType.freeAVars.map(_.name)
      val funAttr = VarStream.forAVars(usedVars.toSet).head
      TFun(tFunParams, ANonUniq, returnType).withAttr(funAttr).generalize(Set.empty)
    }
  }
}
