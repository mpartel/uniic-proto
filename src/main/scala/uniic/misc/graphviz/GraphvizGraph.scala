package uniic.misc.graphviz
import scala.collection.mutable
import scala.sys.process.Process
import java.io.File
import java.io.ByteArrayInputStream
import uniic.misc.FileUtils

class GraphvizGraph(
  var name: String,
  initialAttrs: (String, String)*
) {
  def this() = this(GraphvizGraph.defaultName)

  import GraphvizGraph._

  def setName(name: String): this.type = {
    this.name = name
    this
  }


  val defaultAttrs = Seq("ordering" -> "out")
  val attributes = mutable.Map((defaultAttrs ++ initialAttrs): _*)

  def nodes: Seq[Node] = _nodes.toSeq
  def edges: Set[Edge] = _edges.toSet
  def subgraphs: Seq[GraphvizGraph] = _subgraphs.toSeq

  private val _nodes = mutable.ListBuffer.empty[Node]
  private val _edges = mutable.Set.empty[Edge]
  private val _subgraphs = mutable.ListBuffer.empty[GraphvizGraph]

  private def allNodesIncludingSubgraphs: Iterable[Node] = _nodes.toIterable ++ _subgraphs.flatMap(_.allNodesIncludingSubgraphs)

  private def edgeOrdering: Ordering[Edge] = Ordering.by({e: Edge => e.attributes.getOrElse("label", "")})

  def addNode(node: Node): this.type = {
    _nodes.append(node)
    this
  }

  def addNode(label: String, attrs: (String, String)*): Node = {
    val n = new Node(label)
    addNode(n)
    for ((k, v) <- attrs) {
      n.attributes(k) = v
    }
    n
  }

  def addEdge(edge: Edge): this.type = {
    _edges.add(edge)
    this
  }

  def addEdge(from: Node, to: Node, attrs: (String, String)*): Edge = {
    val e = new Edge(from, to)
    addEdge(e)
    for ((k, v) <- attrs) {
      e.attributes(k) = v
    }
    e
  }

  def addSubgraph(sg: GraphvizGraph) {
    _subgraphs.append(sg)
  }

  def findNode(key: Any): Option[Node] = {
    _nodes.find(_.key == key)
  }

  def findEdge(key: Any): Option[Edge] = {
    _edges.find(_.key == key)
  }

  def toGraphvizSpec: String = {
    val sb = new StringBuilder
    val nodeNameMap = allNodesIncludingSubgraphs.zip(Stream.from(1).map("node" + _)).toMap
    sb.append("digraph ").append(this.name).append(" {\n")
    toGraphvizSpecImpl(sb, nodeNameMap)
    sb.append("}\n")
    sb.toString
  }

  protected def toGraphvizSpecImpl(sb: StringBuilder, nodeNameMap: Map[Node, String]) {
    def putAttr(name: String, value: Any) {
      sb.append(name).append(" = \"").append(value.toString).append("\",")
    }
    def putAttrs(attrs: collection.Map[String, String]) {
      attrs.map { case (k, v) => putAttr(k, v) }
      dropLastComma
    }
    def dropLastComma {
      if (sb(sb.length - 1) == ',') {
        sb.deleteCharAt(sb.length - 1)
      }
    }

    sb.append("graph [")
    putAttrs(this.attributes)
    sb.append("];")

    for (node <- _nodes) {
      sb.append(nodeNameMap(node)).append(" [")
      putAttrs(node.attributes)
      sb.append("] ").append(";\n")
    }

    for (edge <- _edges.toSeq.sorted(edgeOrdering)) {
      sb.append(nodeNameMap(edge.from)).append(" -> ").append(nodeNameMap(edge.to))
      sb.append(" [")
      putAttrs(edge.attributes)
      sb.append("];\n")
    }

    for (subgraph <- _subgraphs) {
      sb.append("subgraph ").append(subgraph.name).append(" {")
      subgraph.toGraphvizSpecImpl(sb, nodeNameMap)
      sb.append("}\n")
    }
  }

  def writeImageToDebugDir(background: Boolean = false) {
    val outfile = debugOutFile
    println("Writing " + outfile)
    val input = new ByteArrayInputStream(this.toGraphvizSpec.getBytes("UTF-8"))
    val process = Process(dotPath, Seq("-Tpng", "-o" + outfile)).#<(input)
    if (background) {
      // TODO: this needs some sort of manager to limit concurrent writes of the same file
      // and to limit the number of concurrent processes.
      process.run()
    } else {
      val exitCode = process.!
      if (exitCode != 0) {
        throw new Exception("Failed to produce graphviz. Is `dot` installed? Exit code: " + exitCode)
      }
    }
  }

  private def debugOutFile = {
    import FileUtils._

    val targetDir = new File(".") / "tmp" / "debug"
    targetDir.mkdirs
    targetDir / this.name + ".png"
  }

  private def dotPath = {
    val candidate = new File("/usr/local/bin/dot") // Not always in PATH, especially on OS X.
    if (candidate.exists()) {
      candidate.getAbsolutePath()
    } else {
      "dot"
    }
  }
}

object GraphvizGraph {
  val defaultName = "g"

  class Node(initialLabel: String, attrs: (String, String)*) {
    var key: Any = null
    val attributes = mutable.Map(attrs: _*)
    attributes.put("label", initialLabel)
  }

  class Edge(val from: Node, val to: Node, attrs: (String, String)*) {
    var key: Any = null
    val attributes = mutable.Map(attrs: _*)
  }
}
