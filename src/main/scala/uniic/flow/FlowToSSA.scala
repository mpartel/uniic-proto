package uniic.flow

import uniic.misc._
import uniic.fun._

object FlowToSSA extends ExceptionContexts {
  def apply(initialFlowGraph: FlowGraph, params: Set[String]): FlowGraph = {
    var flowGraph = initialFlowGraph

    val varIndexer = new NameIndexer
    params.foreach { v => varIndexer.next(v) } // Parameters already have their initial assignment.

    flowGraph = giveVersionNumbersToAssignedVariables(flowGraph, varIndexer)

    flowGraph = convertReadsToUseVersionedVariables(flowGraph)

    flowGraph = IterativeAlgo.untilReady(flowGraph) { g =>
      val sites = findNewPhiSites(g)
      val (g2, done) = addEmptyPhis(g, sites, varIndexer)
      (convertReadsToUseVersionedVariables(g2), done)
    }

    flowGraph = sortPhis(flowGraph) // For less fragile tests

    flowGraph = addReadsToPhis(flowGraph)

    flowGraph
  }

  private def giveVersionNumbersToAssignedVariables(flowGraph: FlowGraph, varIndexer: NameIndexer): FlowGraph = {
    def versionedVar(original: FlowVar) = FlowVar(original.baseName, varIndexer.nextIndex(original.baseName))

    flowGraph.mapBlocks { case (lbl, block) =>
      val ssaBasics = block.basics.map {
        case FlowSetConst(v, c) => FlowSetConst(versionedVar(v), c)
        case FlowSet(v, v2) => FlowSet(versionedVar(v), v2)
        case FlowBorrowingRemoval(v, v2) => FlowBorrowingRemoval(versionedVar(v), v2)
        case FlowCall(v, fv, args) => {
          val borrowings = args.collect {
            case FlowBorrowedArg(in, out) => (in, out)
          }
          CollectionUtil.findDuplicate(borrowings.map(_._1)).foreach { dup =>
            throw new CompilerException("Variable borrowed twice: " + dup)
          }
          CollectionUtil.findDuplicate(borrowings.map(_._2)).foreach { dup =>
            throw new CompilerException("Variable repeated as borrowing `out` variable: " + dup)
          }

          val args2 = args.map {
            case FlowBorrowedArg(in, out) => FlowBorrowedArg(in, versionedVar(out))
            case a => a
          }
          FlowCall(versionedVar(v), fv, args2)
        }
        case FlowMakeTuple(v, args) => FlowMakeTuple(versionedVar(v), args)
        case FlowSplitTuple(vs, v) => FlowSplitTuple(vs.map(versionedVar), v)
      }
      FlowBlock(Seq.empty, ssaBasics, block.jump)
    }
  }

  /** Finds sites where phi statements must be added.
    *
    * See "SSA is Functional Programming" by Andrew W. Appel for an explanation of
    * the algorithm. Basically, given a declaration site in a block B,
    * phis may only be required in the dominance frontier of B.
    *
    * Because adding phis adds new definition sites, this search should be repeated
    * after the previous iteration's phis are added.
    */
  private def findNewPhiSites(flowGraph: FlowGraph): Map[FlowLabel, Set[String]] = {

    def hasPhi(lbl: FlowLabel, varName: String) = {
      flowGraph.blocks.get(lbl) match {
        case Some(block) => block.phis.exists(_.variable.baseName == varName)
        case None => false
      }
    }

    def isLive(lbl: FlowLabel, varName: String) = {
      import FlowLiveVariableAnalysis._
      flowGraph.liveVariableAnalysis.result.blockInputs(lbl).exists {
        case SimpleLiveVar(v) => v.baseName == varName
        case PhiLiveVar(vs) => vs.exists(_.baseName == varName)
      }
    }

    val bindings: Traversable[(FlowLabel, String)] = for {
      (lbl, block) <- flowGraph.blocks
      domFrontLbl <- flowGraph.dominatorTree.dominanceFrontier(lbl)
      definedVarBaseName <- block.latestLocallyDefinedVars.keys
      if !hasPhi(domFrontLbl, definedVarBaseName)
      if isLive(domFrontLbl, definedVarBaseName)
    } yield (domFrontLbl, definedVarBaseName)

    MultiMap.fromTraversable(bindings).toImmutable.withDefaultValue(Set.empty)
  }

  /** Adds phis of the form `x = phi()`. The variables to read are filled in later. */
  private def addEmptyPhis(
    flowGraph: FlowGraph,
    newSites: Map[FlowLabel, Set[String]],
    varIndexer: NameIndexer
  ): (FlowGraph, Boolean) = {
    var changed = false
    val newFlowGraph = flowGraph.mapBlocks { case (lbl, block@FlowBlock(oldPhis, basics, jump)) =>
      newSites.get(lbl) match {
        case Some(vars) => {
          changed = true
          val newPhis = vars.map(v => FlowPhiStmt(FlowVar(v, varIndexer.nextIndex(v)), Seq.empty))
          FlowBlock(oldPhis ++ newPhis, basics, jump)
        }
        case None => block
      }
    }
    (newFlowGraph, !changed)
  }

  private def sortPhis(flowGraph: FlowGraph): FlowGraph = {
    flowGraph.mapBlocks { case (lbl, FlowBlock(phis, basics, jump)) =>
      FlowBlock(phis.sortBy(_.variable), basics, jump)
    }
  }

  private def addReadsToPhis(flowGraph: FlowGraph): FlowGraph = {
    flowGraph.mapBlocks { case (lbl, FlowBlock(phis, basics, jump)) =>
      val newPhis = phis.map { case FlowPhiStmt(result, _) =>
        val varsToRead = flowGraph.predecessors(lbl).flatMap { pred =>
          flowGraph.latestVariableVersions(pred).get(result.baseName)
        }
        FlowPhiStmt(result, varsToRead.toSeq.sortBy(_.version))
      }
      FlowBlock(newPhis, basics, jump)
    }
  }

  private def convertReadsToUseVersionedVariables(flowGraph: FlowGraph): FlowGraph = {
    val latestAtBlockEnd: Map[FlowLabel, Set[FlowVar]] = flowGraph.blocks.map {
      case (lbl, block) => lbl -> block.latestLocallyDefinedVars.values.toSet
    }.toMap

    flowGraph.mapBlocks { case (lbl, block) =>
      val latest = new LatestVariables
      for (a <- flowGraph.dominatorTree.ancestors(lbl).reverse) {
        latest.addAll(latestAtBlockEnd.getOrElse(a, Set.empty))
      }

      val newPhis = block.phis.map {
        case phi@FlowPhiStmt(v, _) => {
          latest.add(v)
          // phi reads already handled in addReadsToPhis
          phi
        }
      }
      val newBasics = block.basics.map {
        case FlowSetConst(v, c) => {
          latest.add(v)
          FlowSetConst(v, c)
        }
        case FlowSet(v, from) => {
          val from2 = latest.get(from.baseName)
          latest.add(v)
          FlowSet(v, from2)
        }
        case FlowBorrowingRemoval(v, from) => {
          val from2 = latest.get(from.baseName)
          latest.add(v)
          FlowBorrowingRemoval(v, from2)
        }
        case FlowCall(v, fv, args) => {
          val fv2 = latest.get(fv.baseName)
          val args2 = args.map {
            case FlowBorrowedArg(in, out) => {
              val a = FlowBorrowedArg(latest.get(in.baseName), out)
              latest.add(out)
              a
            }
            case FlowNormalArg(in) => FlowNormalArg(latest.get(in.baseName))
          }
          latest.add(v)
          FlowCall(v, fv2, args2)
        }
        case FlowMakeTuple(v, args) => {
          val args2 = args.map(a => latest.get(a.baseName))
          latest.add(v)
          FlowMakeTuple(v, args2)
        }
        case FlowSplitTuple(vs, v) => {
          val v2 = latest.get(v.baseName)
          latest.addAll(vs)
          FlowSplitTuple(vs, v2)
        }
      }
      val newJump = block.jump match {
        case j@FlowGoto(_) => j
        case FlowGotoCond(v, l1, l2) => FlowGotoCond(latest.get(v.baseName), l1, l2)
        case FlowReturn(v) => FlowReturn(latest.get(v.baseName))
      }
      FlowBlock(newPhis, newBasics, newJump)
    }
  }

  private class LatestVariables {
    private var _vars = Map.empty[String, FlowVar]
    def vars = _vars

    def add(v: FlowVar) {
      _vars += (v.baseName -> v)
    }

    def addAll(vs: Iterable[FlowVar]) {
      vs.foreach(add(_))
    }

    def contains(v: String) = _vars.contains(v)

    def get(v: String) = {
      if (!_vars.contains(v)) {
        _vars += v -> FlowVar(v, 0) // Free variable
      }
      vars(v)
    }
  }
}
