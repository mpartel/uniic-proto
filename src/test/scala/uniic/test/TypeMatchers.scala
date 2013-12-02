package uniic.test
import org.scalatest.matchers.ClassicMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait TypeMatchers extends ClassicMatchers {
  def beA[T : Manifest] = new Matcher[AnyRef] {
    def apply(left: AnyRef) = MatchResult(
      manifest[T].runtimeClass.isInstance(left),
      left + " was not an instance of " + manifest[T].runtimeClass.getSimpleName,
      left + " was an instance of " + manifest[T].runtimeClass.getSimpleName
    )
  }

  def beAn[T : Manifest] = beA[T]
}
