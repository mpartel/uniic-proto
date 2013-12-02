package uniic.grs
import uniic.misc.Unicode

sealed abstract class RefCount {
  def or(that: RefCount): RefCount = {
    (this, that) match {
      case (SharedRef, _) => SharedRef
      case (_, SharedRef) => SharedRef
      case (UnsharedRef, UnsharedRef) => UnsharedRef
    }
  }
}
case object UnsharedRef extends RefCount {
  override def toString = "" + Unicode.oDotSymbol
}
case object SharedRef extends RefCount {
  override def toString = "" + Unicode.oTimesSymbol
}
