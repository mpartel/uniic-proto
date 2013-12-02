package uniic.misc

class StreamReader[T](val initialStream: Stream[T]) {
  private[this] var _remainingStream = initialStream

  def remainingStream: Stream[T] = _remainingStream

  def peek: T = _remainingStream.head

  def take(): T = {
    val x = _remainingStream.head
    _remainingStream = _remainingStream.tail
    x
  }
}
