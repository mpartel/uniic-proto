package uniic.grs

import uniic.misc.Thunk

/** Graph rewrite system node.
  *
  * All instances are really GrsNodeOf instances. */
sealed trait GrsNode {
  def value: GrsValue
  def children: Seq[GrsNode]
  lazy val edges: Seq[GrsEdge] = children.indices.map(i => GrsEdge(this, i))

  def soleEdge: GrsEdge = {
    assert(edges.size == 1, "A node was expected to have exactly one edge: " + this)
    edges.head
  }
  def soleChild: GrsNode = soleEdge.to

  def downcast[V <: GrsValue : Manifest]: GrsNodeOf[V] = {
    if (manifest[V].runtimeClass.isInstance(value)) {
      this.asInstanceOf[GrsNodeOf[V]]
    } else {
      throw new IllegalArgumentException(
        "Node value " + value + " but expected " + manifest[V].runtimeClass.getSimpleName
      );
    }
  }

  def tryDowncast[V <: GrsValue : Manifest]: Option[GrsNodeOf[V]] = {
    if (manifest[V].runtimeClass.isInstance(value)) {
      Some(this.asInstanceOf[GrsNodeOf[V]])
    } else {
      None
    }
  }

  def cloneWithChildren(newChildren: Seq[GrsNode]): GrsNode
  def lazyCloneWithChildren(newChildren: () => Seq[GrsNode]): GrsNode

  override def toString = "(" + value + ")@" + super.hashCode
}

trait GrsNodeOf[+V <: GrsValue] extends GrsNode {
  def value: V

  def cloneWithChildren(newChildren: Seq[GrsNode]): GrsNodeOf[V] = {
    checkArity(newChildren)
    GrsNode(value, newChildren)
  }

  def lazyCloneWithChildren(newChildren: () => Seq[GrsNode]): GrsNodeOf[V] = {
    GrsNode.applyLazyChildren(value, { () =>
      val nc = newChildren.apply()
      checkArity(nc)
      nc
    })
  }

  private def checkArity(newChildren: Seq[GrsNode]): Unit = {
    if (newChildren.length != value.arity) {
      throw new IllegalArgumentException(
        s"${newChildren.length} new children given, ${value.arity} expected"
      );
    }
  }
}

object GrsNode {
  def apply[V <: GrsValue](_value: V, _children: Seq[GrsNode]): GrsNodeOf[V] = new GrsNodeOf[V] {
    val value = _value
    val children = _children
  }

  def applyLazyChildren[V <: GrsValue](_value: V, _children: () => Seq[GrsNode]): GrsNodeOf[V] = new GrsNodeOf[V] {
    val value = _value
    lazy val children = _children.apply()
  }

  def unapply(node: GrsNode) = Some(node.value)
}
