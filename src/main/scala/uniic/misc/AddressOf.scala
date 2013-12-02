package uniic.misc

/** `AddressOf(a) == AddressOf(b)` iff `a eq b`.
  *
  * Avoid this, it seems haxy. */
final case class AddressOf[+T <: AnyRef](obj: T) {
  override def equals(that: Any) = {
    that match {
      case that: AddressOf[_] => this.obj eq that.obj
      case _ => false
    }
  }
}