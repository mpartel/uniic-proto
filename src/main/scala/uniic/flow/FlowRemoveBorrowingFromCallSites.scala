package uniic.flow

import uniic.misc._

/** Transforms calls from `r := f(a, ...)` where `f` borrows `a, ...`, to `(r, a, ...) := f(a, ...)`. */
trait FlowRemoveBorrowingFromCallSites extends ExceptionContexts {
  def apply(flowGraph: FlowGraph): FlowGraph = {
    var g = flowGraph
    var bs = findBorrowingSites(flowGraph)
    while (!bs.isEmpty) {
      val replacement = computeReplacement(g, bs.head)
      g = doReplacement(g, replacement._1, replacement._2)
      bs = adjustRemainingBorrowingSites(bs.tail, replacement)
    }
    g
  }

  private case class BorrowingSite(addr: FlowStmtAddr, argIndices: Set[Int])

  private def findBorrowingSites(flowGraph: FlowGraph): Seq[BorrowingSite] = {
    flowGraph.blocksInOrder.flatMap {
      case (lbl, block) => {
        block.allStmts.zipWithIndex.flatMap {
          case (FlowCall(_, _, args), stmtIndex) => {
            val argIndices = args.zipWithIndex.collect {
              case (_: FlowBorrowedArg, i) => i
            }
            if (!argIndices.isEmpty) {
              Some(BorrowingSite(FlowStmtAddr(lbl, stmtIndex), argIndices.toSet))
            } else {
              None
            }
          }
          case _ => None
        }
      }
    }
  }

  private def computeReplacement(
    flowGraph: FlowGraph,
    borrowingSite: BorrowingSite
  ): (FlowStmtAddr, Seq[FlowStmt]) = {
    flowGraph.stmtAt(borrowingSite.addr) match {
      case FlowCall(returnVar, f, args) => {
        val f2 = f.mapBaseName(_ + "$nb") // "non-borrowing"
        val tmpReturnVar = f.mapBaseName(_ + "$nbr") // "non-borrowing result"

        val argMap: Map[Int, FlowArg] = args.zipWithIndex.map(_.swap).toMap
        val outArgVars = borrowingSite.argIndices.toSeq.sorted.map(argMap).flatMap(_.maybeOut)

        val removeStmt = FlowBorrowingRemoval(f2, f)
        val newCallStmt = FlowCall(tmpReturnVar, f2, args.map(a => FlowNormalArg(a.in)))
        val splitReturnStmt = FlowSplitTuple(returnVar +: outArgVars, tmpReturnVar)

        val newCode = Seq(removeStmt, newCallStmt, splitReturnStmt)

        (borrowingSite.addr -> newCode)
      }
      case stmt => throw new AssertionError(s"Weird borrowing site: $stmt")
    }
  }

  private def doReplacement(
    flowGraph: FlowGraph,
    addr: FlowStmtAddr,
    newCode: Seq[FlowStmt]
  ): FlowGraph = {
    val oldStmts = flowGraph.blocks(addr.lbl).allStmts
    val newStmts = oldStmts.patch(addr.stmtIndex, newCode, 1)
    flowGraph.mapLabels {
      case lbl if lbl == addr.lbl => FlowBlock.fromStatements(newStmts)
      case lbl => flowGraph.blocks(lbl)
    }
  }

  private def adjustRemainingBorrowingSites(
    borrowingSites: Seq[BorrowingSite],
    replacement: (FlowStmtAddr, Seq[FlowStmt])
  ): Seq[BorrowingSite] = {
    val changeAddr = replacement._1
    val sizeDelta = replacement._2.length - 1
    borrowingSites.map { bs =>
      if (bs.addr.lbl == changeAddr.lbl && bs.addr.stmtIndex > changeAddr.stmtIndex) {
        bs.copy(addr = FlowStmtAddr(bs.addr.lbl, bs.addr.stmtIndex + sizeDelta))
      } else {
        bs
      }
    }
  }
}

object FlowRemoveBorrowingFromCallSites extends FlowRemoveBorrowingFromCallSites with HasDebugVersion[FlowRemoveBorrowingFromCallSites] {
  val withDebug = new FlowRemoveBorrowingFromCallSites with DebugContexts
}
