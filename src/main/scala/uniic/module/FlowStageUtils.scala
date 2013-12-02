package uniic.module

import uniic.flow._
import uniic.types._

trait FlowStageUtils {
  _: Stage =>

  protected def doSSATyping(params: Seq[(String, TFunParam)], wantedReturnType: Option[Type], flowGraph: FlowGraph, useBorrowingSymTab: Boolean): SSATyping.Result = {
    val flowTyping = SSATyping.withDebug(this.isDebugged)
    val symTab = {
      if (useBorrowingSymTab) {
        ctx.symTab.mapValues(_.borrowingType)
      } else {
        ctx.symTab.mapValues(_.nonborrowingType)
      }
    }
    val paramsForTyping = params.map {
      case (p, TFunParam(ty, mode)) => (p, ty, mode)
    }
    flowTyping(
      flowGraph = flowGraph,
      externalSymbols = symTab,
      params = paramsForTyping,
      wantedReturnType = wantedReturnType,
      typeSymTab = ctx.typeSymTab
    )
  }
}
