package uniic.test
import difflib.DiffUtils
import difflib.Patch

/** Wraps the DiffUtils library with methods for Scala collections. */
object ScalaDiffUtils {
  import scala.collection.JavaConverters._

  def diff[A](left: Seq[A], right: Seq[A]): Patch = {
    DiffUtils.diff(left.asJava, right.asJava)
  }

  def diffToString[A](left: Seq[A], right: Seq[A], options: DiffToStringOptions = DiffToStringOptions.default): String = {
    val patch = diff(left, right)
    DiffUtils.generateUnifiedDiff(
      options.originalFilename,
      options.revisedFilename,
      left.map(_.toString).asJava,
      patch,
      options.context
    ).asScala.mkString("\n")
  }

  trait DiffToStringOptions {
    val originalFilename = "orig"
    val revisedFilename = "rev"
    val context = 3
  }

  object DiffToStringOptions {
    val default = new DiffToStringOptions {}
  }
}
