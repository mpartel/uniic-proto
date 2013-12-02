package uniic.grs

object GrsPatternMatcher {
  def apply(patNode: GrsNode, headNode: GrsNode): Option[Map[GrsNode, GrsNode]] = {
    (patNode.value, headNode.value) match {
      case (_: GrsVar, _) => Some(Map(patNode -> headNode))
      case (GrsUnused, _) => Some(Map.empty)
      case (a, b) if a == b && patNode.children.size == headNode.children.size => {
        val emptyMatches = Option(Map.empty[GrsNode, GrsNode])
        patNode.children.zip(headNode.children).foldLeft(emptyMatches) {
          case (Some(ms), (p, h)) => apply(p, h).flatMap(combine(ms, _))
          case (None, _) => None
        }
      }
      case _ => None
    }
  }

  private def combine(left: Map[GrsNode, GrsNode], right: Map[GrsNode, GrsNode]): Option[Map[GrsNode, GrsNode]] = {
    if (left.keys.forall(k => !right.contains(k))) {
      Some(left ++ right)
    } else {
      None
    }
  }
}
