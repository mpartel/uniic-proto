package uniic.misc
import scala.collection.mutable

/** A concrete class that inherits scala.collection.mutable.MultiMap. */
class MultiMap[K, V] extends mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V] {
  def containsBinding(k: K, v: V): Boolean = {
    this.get(k).map(_.contains(v)).getOrElse(false)
  }

  def toImmutable: Map[K, Set[V]] = {
    this.mapValues(_.toSet).toMap
  }
}

/** A factory for MultiMap, which inherit's scala.collection.mutable.MultiMap. */
object MultiMap {
  def empty[K, V] = new MultiMap[K, V]
  def apply[K, V](pairs: (K, V)*) = fromTraversable(pairs)

  def fromTraversable[K, V](pairs: Traversable[(K, V)]) = {
    val mm = new MultiMap[K, V]
    pairs.foreach { case (k, v) => mm.addBinding(k, v) }
    mm
  }

  def reverseImmutable[A, B](m: Map[A, Set[B]]): Map[B, Set[A]] = {
    var result = MultiMap.empty[B, A]
    for ((a, bs) <- m; b <- bs) {
      result.addBinding(b, a)
    }
    result.toImmutable
  }
}
