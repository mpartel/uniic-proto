package uniic.flow

import uniic.types.ParamMode

/** Transforms `return x` to `return (x, a, ...)` where `a, ...` are the
  * function's borrowed parameters. */
trait FlowRemoveBorrowingFromReturnSites {
  def apply(flowGraph: FlowGraph, params: Seq[(String, ParamMode)]): FlowGraph = {
    val borroweds = params.flatMap {
      case (v, m) if m.isBorrowing => Some(FlowVar(v, 0))
      case _ => None
    }

    if (!borroweds.isEmpty) {
      flowGraph.mapBlocks { case (lbl, block) =>
        block.jump match {
          case FlowReturn(r) => {
            val newVar = FlowVar("$returnWithBorrowed", 0)
            block.copy(
              basics = block.basics :+ FlowMakeTuple(newVar, r +: borroweds),
              jump = FlowReturn(newVar)
            )
          }
          case _ => block
        }
      }
    } else {
      flowGraph
    }
  }
}

object FlowRemoveBorrowingFromReturnSites extends FlowRemoveBorrowingFromReturnSites
