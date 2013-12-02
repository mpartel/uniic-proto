package uniic.grs

case class GrsEdge(from: GrsNode, childIndex: Int) {
  def to: GrsNode = from.children(childIndex)

  /** A lazy edge is one that is not evaluated by default. */
  def isLazyEdge: Boolean = from.value.isLazyEdge(childIndex)

  /** A metadata edge is one that refcounting ignores. */
  def isMetadataEdge: Boolean = from.value.isMetadataEdge(childIndex)
}

object GrsEdgeTo {
  def unapply(edge: GrsEdge) = Some(edge.to)
}
