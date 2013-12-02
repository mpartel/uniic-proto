package uniic.misc

class IndexGen(initialIndex: Int = 1) {
  private var _nextIndex = initialIndex

  def next(): Int = {
    val n = _nextIndex
    _nextIndex += 1
    n
  }
}
