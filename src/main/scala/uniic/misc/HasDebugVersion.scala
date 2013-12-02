package uniic.misc

trait HasDebugVersion[T] {
  self: T =>
  val withDebug: T
  def withDebug(debugging: Boolean): T = if (debugging) withDebug else this
}
