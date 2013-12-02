package uniic.misc

trait SimpleGraph[T] {
  val adjacents: (T => Iterable[T])

  def findCycle(root: T): Option[Seq[T]] = {
    var visited = Set.empty[T]
    def visit(x: T): Option[List[T]] = {
      if (x == root) {
        return Some(List(x))
      }
      if (!visited(x)) {
        visited += x
        visitChildren(x)
      } else {
        None
      }
    }

    def visitChildren(x: T): Option[List[T]] = {
      for (a <- adjacents(x)) {
        visit(a) match {
          case Some(result) => return Some(x :: result)
          case None =>
        }
      }
      None
    }

    visitChildren(root).map(_.init)
  }
}

object SimpleGraph {
  def fromFunc[T](f: (T => T)): SimpleGraph[T] = {
    fromFuncToIterable { x: T => Iterable(f(x)) }
  }

  def fromMap[T](f: Map[T, T]): SimpleGraph[T] = {
    fromFuncToIterable { x: T => f.get(x) }
  }

  def fromMultiMap[T](f: Map[T, Iterable[T]]): SimpleGraph[T] = {
    fromFuncToIterable { x: T => f.getOrElse(x, Iterable.empty) }
  }

  def fromFuncToIterable[T](f: (T => Iterable[T])): SimpleGraph[T] = new SimpleGraph[T] {
    val adjacents = f
  }
}
