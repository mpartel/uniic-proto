package uniic.misc

trait Thunk[+T] {
  def value: T
}

object Thunk {
  def apply[T](block: => T): Thunk[T] = new Thunk[T] {
    lazy val value = block
  }
  def fromValue[T](_value: T): Thunk[T] = new Thunk[T] {
    val value = _value
  }
}
