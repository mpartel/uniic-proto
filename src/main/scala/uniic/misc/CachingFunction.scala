package uniic.misc

class CachingFunction[A, B](protected val f: A => B, toStringFunc: () => String = null) extends (A => B) {
  @volatile private[this] var cache = Map.empty[A, B]

  def apply(a: A): B = {
    cache.get(a) match {
      case Some(b) => b
      case None => {
        val b = f(a)
        cache = cache + (a -> b)
        b
      }
    }
  }

  override def toString = if (toStringFunc != null) toStringFunc() else "Cached(" + f.toString + ")"
}

object CachingFunction {
  def apply[A, B](f: A => B) = new CachingFunction(f)
  def apply[A, B](f: A => B, toStringFunc: () => String) = new CachingFunction(f, toStringFunc)
}