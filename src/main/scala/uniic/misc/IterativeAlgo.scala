package uniic.misc

object IterativeAlgo {
  /** Applies `x = f(x)` until `x == f(x)`. */
  def untilNoChange[T](x: T)(f: T => T): T = {
    var prev = x
    var done = false
    while (!done) {
      var next = f(prev)
      if (next == prev) {
        done = true
      } else {
        prev = next
      }
    }
    return prev
  }

  /** Applies `x = f(x)` until `x.size` does not grow. */
  def untilNoneAdded[T <: { def size: Int }](x: T)(f: T => T): T = {
    var prev = x
    var done = false
    while (!done) {
      var next = f(prev)
      if (next.size <= prev.size) {
        done = true
      }
      prev = next
    }
    return prev
  }

  /** Applies `(x, ready) = f(x)` until `ready` is true. */
  def untilReady[T](x0: T)(f: T => (T, Boolean)): T = {
    var ready = false
    var x = x0
    while (!ready) {
      val ret = f(x)
      x = ret._1
      ready = ret._2
    }
    x
  }
}