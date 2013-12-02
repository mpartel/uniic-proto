package uniic.flow
import uniic.misc._
import uniic.misc.graphviz._

/** A flow graph of FlowBlocks, possibly in SSA form. */
class FlowGraph(val blocksInOrder: Seq[(FlowLabel, FlowBlock)], val start: FlowLabel) {
  import FlowGraph.{entry, exit, GraphvizConf}

  override def equals(thatObj: Any): Boolean = {
    thatObj match {
      case that: FlowGraph => (this eq that) || (this.blocksInOrder == that.blocksInOrder && this.start == that.start)
      case _ => false
    }
  }

  override def hashCode = blocksInOrder.hashCode + start.hashCode

  lazy val statements: Seq[FlowStmt] = {
    blocksInOrder.flatMap { case (lbl, FlowBlock(phis, basics, jump)) =>
      Seq(lbl) ++ phis ++ basics ++ Seq(jump)
    }
  }

  lazy val blocks = blocksInOrder.toMap

  def allStmts: Iterable[FlowStmt] = for (block <- blocksInOrder.view.map(_._2); stmt <- block.allStmts) yield stmt
  def allStmtAddrs: Iterable[FlowStmtAddr] = for ((lbl, block) <- blocksInOrder; i <- block.allStmts.indices) yield FlowStmtAddr(lbl, i)
  def allStmtsWithLabels: Iterable[FlowStmt] = blocksInOrder.flatMap { case (lbl, b) => lbl +: b.allStmts }
  def allStmtsWithLabelsSeq: Seq[FlowStmt] = allStmtsWithLabels.toSeq

  lazy val blockLabels = blocksInOrder.map(_._1)
  lazy val allLabels = entry +: blockLabels :+ exit

  def mapLabels(f: FlowLabel => FlowBlock) = {
    new FlowGraph(blockLabels.map(lbl => lbl -> f(lbl)), start)
  }

  def mapBlocks(f: (FlowLabel, FlowBlock) => FlowBlock) = {
    new FlowGraph(blocksInOrder.map { case (lbl, block) => lbl -> f(lbl, block) }, start)
  }

  lazy val successors: Map[FlowLabel, Seq[FlowLabel]] = {
    val fromBlocks = blocks.map { case (lbl, block) =>
      val ss = block.jump match {
        case FlowGoto(dest) => Seq(dest)
        case FlowGotoCond(_, dest1, dest2) => Seq(dest1, dest2)
        case FlowReturn(_) => Seq.empty
      }
      (lbl -> ss)
    }
    (fromBlocks + (entry -> Seq(start)) + (exit -> Seq.empty)) ++ returners.map(_ -> Seq(exit))
  }

  lazy val predecessors: Map[FlowLabel, Set[FlowLabel]] = {
    val fromBlocks = blocks.map { case (lbl, block) =>
      val preds = successors.iterator.filter { case (_, predDests) => predDests.contains(lbl) }.map(_._1).toSet
      (lbl -> preds)
    }
    fromBlocks + (exit -> returners) + (entry -> Set.empty)
  }

  lazy val successorsTransitiveClosure = {
    CollectionUtil.transitiveClosure(successors.mapValues(_.toSet))
  }

  lazy val predecessorsTransitiveClosure = {
    CollectionUtil.transitiveClosure(predecessors)
  }


  private lazy val varUsageSets: Map[FlowLabel, (Set[FlowVar], Set[FlowVar], Set[FlowVar])] = {
    blocks.iterator.map { case (lbl, block) =>
      var allRead = Set.empty[FlowVar]
      var required = Set.empty[FlowVar]
      var written = Set.empty[FlowVar]

      for (stmt <- block.allStmts) {
        for (v <- stmt.readVars) {
          allRead += v
          if (!written(v)) {
            required += v
          }
        }
        written ++= stmt.resultVars
      }
      (lbl -> (allRead, required, written))
    }.toMap
  }

  lazy val varsRead: Map[FlowLabel, Set[FlowVar]] = varUsageSets.mapValues(_._1)
  lazy val varsRequired: Map[FlowLabel, Set[FlowVar]] = varUsageSets.mapValues(_._2)
  lazy val varsWritten: Map[FlowLabel, Set[FlowVar]] = varUsageSets.mapValues(_._3)

  /** All variables read at some point by some block. */
  lazy val allVarsRead: Set[FlowVar] = varsRead.values.foldLeft(Set.empty[FlowVar])(_ ++ _)

  /** All variables written at some point by some block. */
  lazy val allVarsWritten: Set[FlowVar] = varsWritten.values.foldLeft(Set.empty[FlowVar])(_ ++ _)

  /** All variables read or written at some point by some block. */
  lazy val allVars: Set[FlowVar] = allVarsRead ++ allVarsWritten

  lazy val returners: Set[FlowLabel] = blocks.iterator.filter {
    case (_, block) => block.jump.isInstanceOf[FlowReturn]
  }.map(_._1).toSet

  /** The set of returning blocks with the variables that occur in return statements. */
  lazy val returnersAndReturnedVars: Set[(FlowLabel, FlowVar)] = returners.map { lbl =>
    blocks(lbl).jump match {
      case FlowReturn(v) => lbl -> v
      case _ => throw new CompilerException("Internal error")
    }
  }

  lazy val dominatorTree = DominatorTree(this)

  /** Maps a `lbl` and a `baseName` to the latest version FlowVar named `baseName` defined by the end of `lbl`.
    * The definition may be in `lbl` or one of its dominators. */
  lazy val latestVariableVersions: Map[FlowLabel, Map[String, FlowVar]] = {
    var result = Map.empty[FlowLabel, Map[String, FlowVar]]
    def recurse(lbl: FlowLabel, fromDominators: Map[String, FlowVar]) {
      if (blocks.contains(lbl)) {
        val fromThisBlock = blocks(lbl).latestLocallyDefinedVars
        val varsHere = fromDominators ++ fromThisBlock
        result += (lbl -> varsHere)
        dominatorTree.children(lbl).foreach(recurse(_, varsHere))
      }
    }
    recurse(start, Map.empty)
    result.toMap.withDefault(Map.empty)
  }

  lazy val reachingDefinitionsAnalysis = new FlowReachingDefinitionsAnalysis(this)
  lazy val liveVariableAnalysis = new FlowLiveVariableAnalysis(this)
  lazy val readCountAnalysis = new FlowReadCountAnalysis(this)

  /** Variables that may be read before use. */
  lazy val freeVarsSeq: Seq[FlowVar] = usesBeforeDefinitions.map(_._1)
  lazy val freeVars: Set[FlowVar] = freeVarsSeq.toSet

  private lazy val usesBeforeDefinitions: Seq[(FlowVar, FlowLabel)] = {
    val resultAsSet = FlowUseBeforeDefinitionAnalysis(this, Set.empty)
    resultAsSet.toSeq.sortBy(_._2)
  }

  def getStmtAt(addr: FlowStmtAddr): Option[FlowStmt] = {
    blocks.get(addr.lbl).flatMap { b =>
      b.allStmts.lift(addr.stmtIndex)
    }
  }

  def stmtAt(addr: FlowStmtAddr): FlowStmt = {
    getStmtAt(addr).getOrElse(throw new CompilerException("Invalid FlowStmtAddr: " + addr))
  }

  def assigningStmtAddr(v: FlowVar): FlowStmtAddr = {
    getAssigningStmtAddrs(v) match {
      case Seq(a) => a
      case Seq() => throw new CompilerException("No assignments of " + v)
      case _ => throw new CompilerException("More than one assignment of " + v)
    }
  }

  def getAssigningStmtAddrs(v: FlowVar): Seq[FlowStmtAddr] = {
    allStmtAddrs.filter(a => stmtAt(a).resultVars.contains(v)).toSeq
  }

  def toGraphviz: GraphvizGraph = toGraphviz(GraphvizConf.default)
  def toGraphviz(conf: GraphvizConf): GraphvizGraph = toGraphvizAndNodeMap(conf)._1

  def toGraphvizAndNodeMap(conf: GraphvizConf): (GraphvizGraph, Map[FlowLabel, GraphvizGraph.Node]) = {
    val gg = new GraphvizGraph
    var nodeMap = Map.empty[FlowLabel, GraphvizGraph.Node]
    var indexGen = new IndexGen(1)

    val entrySubgraph = new GraphvizGraph("entrySG", "rank" -> "min")
    val entryNode = entrySubgraph.addNode("entry", "shape" -> "point")
    nodeMap += (FlowGraph.entry -> entryNode)
    gg.addSubgraph(entrySubgraph)

    val exitSubgraph = new GraphvizGraph("exitSG", "rank" -> "max")
    val exitNode = exitSubgraph.addNode("entry", "shape" -> "point")
    nodeMap += (FlowGraph.exit -> exitNode)
    gg.addSubgraph(exitSubgraph)

    for ((lbl, block) <- blocksInOrder) {
      val graphvizLabel = conf.blockLabel(lbl, block)
      val node = gg.addNode(graphvizLabel)
      node.key = lbl
      node.attributes("shape") = "rectangle"
      node.attributes("fontname") = "monospace"
      nodeMap += (lbl -> node)
    }

    for ((from, succs) <- successors; succ <- succs) {
      gg.addEdge(nodeMap(from), nodeMap(succ))
    }
    (gg, nodeMap)
  }
}


object FlowGraph {
  /** A special label that is the root of every FlowGraph. */
  val entry = FlowLabel("__entry")
  /** A special label that is the successor of every block ending in a return statement. */
  val exit = FlowLabel("__exit")

  trait GraphvizConf {
    def blockLabel(lbl: FlowLabel, block: FlowBlock) = lbl + "\\l" + block.allStmts.mkString("\\l") + "\\l"
  }

  object GraphvizConf {
    val default = new GraphvizConf {}
  }

  def fromStatements(input: Seq[FlowStmt]): FlowGraph = {
    FlowGraphParsers.parseGraph(input)
  }
}
