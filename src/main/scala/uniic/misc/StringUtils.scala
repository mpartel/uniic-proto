package uniic.misc
import java.util.regex.Pattern

object StringUtils {
  def indent(s: String, spaces: Int): String = {
    val indent = " " * spaces
    s.lines.mkString(indent, "\n" + indent, "")
  }

  private val whitespace = Pattern.compile("\\s+", Pattern.MULTILINE)

  def collapseWhitespace(s: String): String = {
    whitespace.matcher(s).replaceAll(" ")
  }

  def formatPairs[T, U](pairs: Iterable[(T, U)], sepInPair: String = " -> ", sepBetweenPair: String = ", "): String = {
    pairs.map { case (a, b) => a + sepInPair + b }.mkString(sepBetweenPair)
  }

  def truncate(s: String, maxLength: Int, after: String = "...") = {
    if (s.length > maxLength) {
      if (after.length < maxLength) {
        s.substring(0, maxLength - after.length) + after
      } else {
        after.substring(0, maxLength)
      }
    } else {
      s
    }
  }
}