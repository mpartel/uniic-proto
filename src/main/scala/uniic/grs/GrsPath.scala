package uniic.grs

private case class GrsPath(edges: Seq[GrsEdge]) {
  lazy val nodes: Seq[GrsNode] = {
    if (edges.isEmpty) {
      Seq.empty
    } else {
      edges.map(_.from) :+ edges.last.to
    }
  }

  def :+(edge: GrsEdge) = {
    if (edges.isEmpty || edge.from == edges.last.to) {
      GrsPath(edges :+ edge)
    } else {
      throw new IllegalArgumentException(edge.toString)
    }
  }

  def isEmpty = edges.isEmpty

  override def toString = {
    if (edges.isEmpty) {
      "GrsPath()"
    } else {
      val init = edges.map {
        case GrsEdge(from, i) => s"$from -($i)> "
      }
      "GrsPath(" + init.mkString + nodes.last + ")"
    }
  }
}

private object GrsPath {
  val empty = GrsPath(Seq.empty)
}
