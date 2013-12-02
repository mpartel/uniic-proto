package uniic.misc.profiling

trait ProfileLog {
  def begin(label: String)
  def end(label: String, timeElapsed: Long)
}

class SimpleDebugLog(out: java.io.PrintStream) extends ProfileLog {
  private var nesting = 0
  private def indentation = ("  " * nesting) + "[" + nesting + "] "
  def begin(label: String) {
    out.println(indentation + "BEGIN " + label)
    nesting += 1
  }
  def end(label: String, timeElapsed: Long) {
    nesting -= 1
    out.println(indentation + "END " + label + " -- " + timeElapsed + " ms")
  }
}

class Profiling[T](val log: ProfileLog, val label: String) {
  def apply[T](block: => T) = {
    var err: Option[Throwable] = None
    var result: Option[T] = None

    val before = System.currentTimeMillis
    log.begin(label)

    try {
      result = Some(block)
    } catch {
      case e: Throwable => {
        err = Some(e)
      }
    }

    val after = System.currentTimeMillis
    val elapsed = after - before
    log.end(label, elapsed)

    (err, result) match {
      case (Some(e), None) => throw e
      case (None, Some(r)) => r
      case _ => throw new AssertionError
    }
  }
}

object Profiling {
  val debugLog = new SimpleDebugLog(System.err)

  def debug[T](label: String) = new Profiling(debugLog, label)
}
