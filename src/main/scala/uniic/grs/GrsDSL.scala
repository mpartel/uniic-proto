package uniic.grs

/** Provides the syntax `a := GrsSomeValue | (b, c)`
  * where the keys a, b and c can be of whatever type. */
trait GrsDSL {
  /** Implementation detail. */
  implicit class GrsKey[K](key: K) {
    def :=(rhs: GrsValue) = GrsSpec(key, rhs, Seq.empty)
    def :=(rhs: GrsValueWithChildren[K]) = GrsSpec(key, rhs.value, rhs.children)
  }

  /** Implementation detail. */
  case class GrsValueWithChildren[+K](value: GrsValue, children: Seq[K])

  /** Implementation detail. */
  implicit class GrsValueWrapper(value: GrsValue) {
    def |[K](children: K*) = GrsValueWithChildren(value, children)
  }

  /** Implementation detail. */
  case class GrsSpec[+K](key: K, value: GrsValue, children: Seq[K])

  /** Call this. */
  def mkGrs[K](specs: GrsSpec[K]*): Grs = {
    mkGrsAndNodeMap(specs: _*)._1
  }

  /** Call this if you also want a key-to-node map. */
  def mkGrsAndNodeMap[K](specs: GrsSpec[K]*): (Grs, Map[K, GrsNode]) = {
    if (specs.isEmpty) {
      throw new IllegalArgumentException("Cannot construct an empty GRS")
    }

    lazy val nodes: Map[K, GrsNode] = {
      specs.map(s => s.key -> GrsNode.applyLazyChildren(s.value, getChildren(s.children))).toMap
    }
    def getChildren(childKeys: Seq[K])(): Seq[GrsNode] = {
      childKeys.map { k: K =>
        nodes.getOrElse(k, throw new IllegalArgumentException(s"Invalid child key `$k` in GRS spec"))
      }
    }

    val rootKey = specs.head.key
    (Grs(nodes(rootKey)), nodes)
  }
}
