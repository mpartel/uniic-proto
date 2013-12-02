package uniic.flow
import uniic.fun._
import uniic.fun.tr._
import uniic.imp._
import uniic.misc._
import uniic.types._

/** Converts an SSA program into a functional program as outlined in Appel's "SSA is Functional programming".
  *
  * We transform one block at a time, converting set statements into let definitions,
  * unconditional jumps to function calls and
  * conditional jumps if-else expressions of function calls.
  *
  * Blocks are converted into functions.
  * These functions take as arguments the variables assigned by their phi statements.
  * The function definitions are nested so that variable definitions in dominating nodes
  * can be used from closures without argument passing.
  *
  * Some constraints for nesting the functions are:
  * (1) a function must be nested above all its predecessors, because the predecessors (and nothing else) can call it.
  * (2) a function should see the variables that are visible to all its predecessors.
  * (3) a function should be defined in only one place.
  *
  * These constraints are satisfied by nesting each function inside
  * the function generated from its immediate dominator. */
object SSAToFun {
  def apply(flowGraph: FlowGraph): FunExpr = {

    val toLabelVar: Map[FlowLabel, FunVar] = flowGraph.allLabels.map(l => l -> FunVar("$$" + l.name)).toMap
    val toFunVar: Map[FlowVar, FunVar] = flowGraph.allVars.map(v => v -> FunVar(v.encodedName)).toMap

    def convertBody(lbl: FlowLabel): FunLambda = {
      val block = flowGraph.blocks(lbl)

      val dominatedLbls = flowGraph.dominatorTree.children(lbl) - FlowGraph.exit
      val nestedLets: Seq[(String, FunExpr)] = dominatedLbls.map { dominatedLbl =>
        toLabelVar(dominatedLbl).name -> convertBody(dominatedLbl)
      }.toSeq.sortBy(_._1)

      val jumpExpr: FunExpr = block.jump match {
        case FlowReturn(v) => toFunVar(v)
        case FlowGoto(callee) => convertJumpTo(lbl, callee)
        case FlowGotoCond(v, callee1, callee2) => {
          FunIfThenElse(toFunVar(v), convertJumpTo(lbl, callee1), convertJumpTo(lbl, callee2))
        }
      }

      val afterBasics = FunLet.maybe(nestedLets, jumpExpr)

      val bodyWithNestedLets = block.basics.foldRight(afterBasics: FunExpr) { case (l, r) => convertBasicStmt(l, r) }
      val body = MergeLets(bodyWithNestedLets)

      val paramVars = block.phis.map(_.variable)
      val paramNames = paramVars.map(_.encodedName)
      FunLambda(paramNames.map(_ -> MissingTypeAnnotation), body)
    }

    def convertBasicStmt(stmt: FlowBasicStmt, cont: FunExpr): FunExpr = {
      implicit def toFunVar(v: FlowVar): FunVar = FunVar(v.encodedName)
      implicit def toFunVarSeq(vs: Seq[FlowVar]): Seq[FunVar] = vs.map(toFunVar)

      def let(v: FlowVar, e: FunExpr) = FunLet(Seq(v.encodedName -> e), cont)

      stmt match {
        case FlowSetConst(v, const) => let(v, convertConst(const))
        case FlowSet(v, from) => let(v, from)
        case FlowBorrowingRemoval(v, from) => let(v, from) // No borrowing in the functional world
        case FlowCall(v, fv, args) => let(v, FunApply(fv, args.map(convertArg)))
        case FlowMakeTuple(v, args) => let(v, FunTupleExpr(args))
        case FlowSplitTuple(vs, tupleVar) => {
          val pattern = PatTuple(vs.map(v => PatVar(v.encodedName)))
          FunMatch(FunVar(tupleVar.encodedName), Seq(FunCase(pattern, cont)))
        }
      }
    }

    def convertArg(arg: FlowArg): FunExpr = {
      arg match {
        case FlowBorrowedArg(in, out) => {
          throw new CompilerException(
            "Borrowing argument passed to SSAToFun. Borrowing removal was not applied or is buggy."
          )
        }
        case FlowNormalArg(in) => FunVar(in.encodedName)
      }
    }

    def convertJumpTo(callerLbl: FlowLabel, calleeLbl: FlowLabel): FunExpr = {
      val calleeBlock = flowGraph.blocks(calleeLbl)
      val varsToPass = calleeBlock.phis.map { case FlowPhiStmt(dest, callerVars) =>
        val availableVars = flowGraph.latestVariableVersions(callerLbl).values.toSet
        val matchingVars = callerVars.filter(v => availableVars.contains(v))
        matchingVars match {
          case Seq() => throw new CompilerException("No variable to pass to phi'ed variable " + dest)
          case Seq(v) => v
          case _ => throw new CompilerException("Ambiguous phi for " + dest + " (" + matchingVars.mkString(", ") + ")")
        }
      }
      FunApply(toLabelVar(calleeLbl), varsToPass.map(toFunVar))
    }

    def convertConst(c: ImpConst): FunExpr = {
      c match {
        case ImpUnit => FunUnit
        case ImpBool(v) => FunBool(v)
        case ImpInt(v) => FunInt(v)
      }
    }

    convertBody(flowGraph.start).body
  }
}