package uniic.imp
import uniic.flow._
import uniic.fun._
import uniic.types._
import uniic.misc._
import scala.collection.mutable

/** Converts an imperative program into a (non-SSA) flowgraph.
  *
  * The result is not yet in SSA form as all its variables have version 0
  * and there are no phi statements. See `FlowToSSA` for the SSA conversion. */
object ImpToFlow {
  def apply(stmt: ImpStmt): Seq[FlowStmt] = {

    val internalVarIndexer = new NameIndexer
    def newInternalVar(baseName: String) = {
      val index = internalVarIndexer.nextIndex(baseName)
      val name = "$" + baseName + "$" + index
      FlowVar(name, 0)
    }

    val labelIndexer = new NameIndexer
    def newLabel(baseName: String) = FlowLabel(labelIndexer.next(baseName))

    def transform(stmt: ImpStmt): Seq[FlowStmt] = {
      stmt match {
        case ImpReturn(expr) => {
          val resultVar = newInternalVar("returnValue")
          val exprFlow = transformExpr(resultVar, expr)
          exprFlow :+ FlowReturn(resultVar)
        }
        case ImpSet(ImpVar(v), expr) => {
          transformExpr(FlowVar(v, 0), expr)
        }
        case ImpSplitTuple(vs, expr) => {
          val tempVar = newInternalVar("split")
          val exprFlow = transformExpr(tempVar, expr)
          exprFlow :+ FlowSplitTuple(vs.map(v => FlowVar(v.name, 0)), tempVar)
        }
        case ImpIf(condExpr, thenBody, elseBody) => {
          val thenLabel = newLabel("then")
          val elseLabel = newLabel("else")
          val endLabel = newLabel("ifEnd")
          val condVar = newInternalVar("ifHead")
          val condFlow = transformExpr(condVar, condExpr)
          val thenFlow = transform(thenBody)
          val elseFlow = transform(elseBody)
          val maybeThenGoto = maybeJump(thenFlow, endLabel)
          condFlow ++ Seq(
            FlowGotoCond(condVar, thenLabel, elseLabel),
            thenLabel
          ) ++ thenFlow ++ maybeThenGoto ++ Seq(
            elseLabel
          ) ++ elseFlow ++ Seq(
            endLabel
          )
        }
        case ImpWhile(condExpr, body) => {
          val headLabel = newLabel("whileHead")
          val bodyLabel = newLabel("whileBody")
          val endLabel = newLabel("whileEnd")
          val condVar = newInternalVar("whileCondition")
          val condFlow = transformExpr(condVar, condExpr)
          val bodyFlow = transform(body)
          val maybeBodyGoto = maybeJump(bodyFlow, headLabel)
          val flow = Seq(
            headLabel
          ) ++ condFlow ++ Seq(
            FlowGotoCond(condVar, bodyLabel, endLabel),
            bodyLabel
          ) ++ bodyFlow ++ maybeBodyGoto ++ Seq(
            endLabel
          )
          flow
        }
        case ImpBlock(stmts) => {
          stmts.flatMap(transform(_))
        }
        case e: ImpExpr => transformExpr(newInternalVar("unused"), e)
      }
    }

    def transformExpr(resultVar: FlowVar, impExpr: ImpExpr): Seq[FlowStmt] = {
      impExpr match {
        case c: ImpConst => Seq(FlowSetConst(resultVar, c))
        case ImpVar(v) => Seq(FlowSet(resultVar, FlowVar(v, 0)))
        case ImpCall(funcExpr, args) => {
          val (funcVar, funcStmts) = transformExprUnlessVar(funcExpr, "func")
          val (argVars, argStmts) = transformFunctionArgs(args, "arg")
          funcStmts ++ argStmts :+ FlowCall(resultVar, funcVar, argVars)
        }
        case ImpMakeTuple(args) => {
          val (argVars, argExprs) = transformArgs(args, "tuple")
          argExprs :+ FlowMakeTuple(resultVar, argVars)
        }
      }
    }

    def transformExprUnlessVar(impExpr: ImpExpr, varBaseName: String): (FlowVar, Seq[FlowStmt]) = {
      impExpr match {
        case ImpVar(v) => (FlowVar(v, 0), Seq.empty)
        case e => {
          val v = newInternalVar(varBaseName)
          (v, transformExpr(v, e))
        }
      }
    }

    def transformArgs(impExprs: Seq[ImpExpr], argBaseName: String): (Seq[FlowVar], Seq[FlowStmt]) = {
      val (vars, stmts) = impExprs.zipWithIndex.map {
        case (e, i) => transformExprUnlessVar(e, argBaseName + i)
      }.unzip
      (vars, stmts.flatten)
    }

    def transformFunctionArgs(impArgs: Seq[ImpArg], argBaseName: String): (Seq[FlowArg], Seq[FlowStmt]) = {
      val (exprs, modes) = impArgs.map {
        case ImpBorrowedArg(v) => (v, MBorrowed)
        case ImpNormalArg(e) => (e, MNone)
      }.unzip
      transformArgs(exprs, argBaseName) match {
        case (vars, stmts) => {
          val args = vars.zip(modes).map {
            case (v, MBorrowed) => FlowBorrowedArg(v, v)
            case (v, MNone) => FlowNormalArg(v)
          }
          (args, stmts)
        }
      }
    }

    def maybeJump(flow: Seq[FlowStmt], lbl: FlowLabel): Option[FlowJumpStmt] = {
      flow.lastOption match {
        case Some(_: FlowJumpStmt) => None
        case _ => Some(FlowGoto(lbl))
      }
    }

    def fixFinalResult(_statements: Seq[FlowStmt]): Seq[FlowStmt] = {
      def ensureStartsWithLabel(stmts: Seq[FlowStmt]) =  {
        if (stmts.isEmpty || !stmts.head.isInstanceOf[FlowLabel]) {
          FlowLabel("_start") +: stmts
        } else {
          stmts
        }
      }

      def ensureEndsInJump(stmts: Seq[FlowStmt]) = {
        if (!stmts.last.isInstanceOf[FlowJumpStmt]) {
          stmts ++ Seq(
            FlowSetConst(FlowVar("$defaultReturn", 0), ImpUnit),
            FlowReturn(FlowVar("$defaultReturn", 0))
          )
        } else {
          stmts
        }
      }

      def addGotosToLabelFallThroughs(stmts: Seq[FlowStmt]): Seq[FlowStmt] = {
        stmts.zip(stmts.tail).flatMap {
          case (nonJump, lbl: FlowLabel) if !nonJump.isInstanceOf[FlowJumpStmt] => {
            Seq(nonJump, FlowGoto(lbl))
          }
          case (s, _) => Seq(s)
        } :+ stmts.last
      }

      def dropUnusedBlocks(stmts: Seq[FlowStmt]): Seq[FlowStmt] = {
        val allTargetLabels: Set[FlowLabel] = stmts.map {
          case FlowGoto(lbl) => Set(lbl)
          case FlowGotoCond(_, lbl1, lbl2) => Set(lbl1, lbl2)
          case _ => Set.empty
        }.foldLeft(Set.empty[FlowLabel])(_ ++ _) + stmts.head.asInstanceOf[FlowLabel]

        var keeping = true
        var result = Seq.empty[FlowStmt]
        for (stmt <- stmts) {
          stmt match {
            case lbl: FlowLabel => {
              keeping = allTargetLabels.contains(lbl)
            }
            case _ =>
          }
          if (keeping) {
            result :+= stmt
          }
          if (stmt.isInstanceOf[FlowJumpStmt]) {
            keeping = true
          }
        }
        result
      }

      var result = _statements
      result = ensureStartsWithLabel(result)
      result = ensureEndsInJump(result)
      result = addGotosToLabelFallThroughs(result)
      result = dropUnusedBlocks(result)
      result
    }

    fixFinalResult(transform(stmt))
  }
}