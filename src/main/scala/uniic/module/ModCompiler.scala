package uniic.module

import uniic.imp._
import uniic.fun._
import uniic.fun.analysis.FunTree
import uniic.flow._
import uniic.grs._
import uniic.types._
import uniic.misc._
import uniic.stdlib.Stdlib

trait ModCompiler extends ExceptionContexts {
  import AsPartialFunction.{apply => toPf}

  val stageDebugSettings = Stage.DebugSettings.noDebug

  def compile(
    initialSymTab: Map[String, Stage.SymTabEntry],
    uncompiled: UncompiledModule
  ): CompiledModule = {
    val typeSymTab = Stdlib.typeSymTab  // Hard-coded for now
    val typeDeclarations = uncompiled.entries.map { case (name, _, typeExpr) =>
      name -> TypeEval.evalType(typeSymTab, typeExpr).generalize(Set.empty)
    }

    val localSymTab = typeDeclarations.map {
      case (name, ts) => name -> Stage.SymTabEntry(GrsExternal(name), ts, TypeBorrowingRemoval(ts))
    }

    val symTab = initialSymTab ++ localSymTab

    val initialStages: Seq[(String, Stage)] = uncompiled.entries.map { case (name, entry, _) =>
      val debugSettings = stageDebugSettings.copy(
        graphvizFilePrefix = name + "_" + stageDebugSettings.graphvizFilePrefix
      )
      val initialCtx = Stage.Context(symTab, typeSymTab, debugSettings)
      val initialStage = mkInitialStage(initialCtx, entry)
      name -> initialStage
    }

    val compiledEntries: Seq[CompiledModule.Entry] = initialStages.map { case (name, initialStage) =>
      context(s"compiling `$name`") {
        val borrowingStage = initialStage.to[BorrowingTypedSSAStage]
        val finalStage = borrowingStage.to[TypedGrsStage]
        val borrowingType = borrowingStage.borrowingSsaTypingResult.functionType
        val finalType = finalStage.rootType
        CompiledModule.Entry(name, finalStage.grs, borrowingType, finalType)
      }
    }

    new CompiledModule(uncompiled.name, compiledEntries)
  }

  private def mkInitialStage(ctx: Stage.Context, entry: UncompiledModEntry): Stage = {
    entry match {
      case UncompiledModImpEntry(impToplevel) => ImpStage(ctx, impToplevel)
      case UncompiledModFunEntry(funExpr) => FunStage(ctx, new FunTree(funExpr), None)
    }
  }
}

object ModCompiler extends ModCompiler with HasDebugVersion[ModCompiler] {
  trait WithDebug extends ModCompiler with DebugContexts {
    override val stageDebugSettings = Stage.DebugSettings.fullDebug
  }
  val withDebug = new WithDebug {}
}
