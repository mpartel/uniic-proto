package uniic.misc
import scala.collection.immutable.ListSet
import scala.collection.mutable

object CollectionUtil {
  def hasDuplicates[T](c: Iterable[T]) = findDuplicate(c).isDefined
  def findDuplicate[T](c: Iterable[T]): Option[T] = c.find(x => c.count(_ == x) > 1)
  def uniq[T](c: Seq[T]) = ListSet(c: _*).toSeq.reverse

  def splitEvenAndOdd[T](c: Seq[T]): (Seq[T], Seq[T]) = {
    c.length match {
      case 0 => (Seq.empty, Seq.empty)
      case 1 => (Seq(c(0)), Seq.empty)
      case 2 => (Seq(c(0)), Seq(c(1)))
      case _ => {
        val (evens, odds) = splitEvenAndOdd(c.drop(2))
        (c(0) +: evens, c(1) +: odds)
      }
    }
  }

  def reverseMap[K, V](map: Map[K, V]): Map[V, Set[K]] = {
    var result = new MultiMap[V, K]
    for ((k, v) <- map) {
      result.addBinding(v, k)
    }
    result.toImmutable
  }

  def transitiveClosure[T](original: Map[T, Set[T]]): Map[T, Set[T]] = {
    val result = MultiMap.empty[T, T]
    for (start <- original.keys) {
      var seen = Set.empty[T]
      var queue = mutable.Queue.empty[T]
      original.getOrElse(start, Set.empty).foreach(queue.enqueue(_))
      while (!queue.isEmpty) {
        val x = queue.dequeue()
        result.addBinding(start, x)
        if (!seen(x)) {
          seen += x
          queue.enqueue(original.getOrElse(x, Set.empty).toSeq: _*)
        }
      }
    }
    result.toImmutable
  }

  def pairsToPrettyString[K, V](map: Iterable[(K, V)], sort: Boolean = true, sep: String = "\n"): String = {
    pairsToPrettyLines(map).mkString(sep)
  }

  def pairsToPrettyLines[K, V](pairs: Iterable[(K, V)], sort: Boolean = true): Seq[String] = {
    var strPairs = pairs.map { case (k, v) => k.toString -> v.toString }.toSeq
    if (sort) {
      strPairs = strPairs.sortBy(_._1)
    }
    val maxKeyLength = if (strPairs.isEmpty) 0 else strPairs.map(_._1.length).max
    strPairs.map { case (k, v) => k.padTo(maxKeyLength, ' ') + " -> " + v }
  }
}
