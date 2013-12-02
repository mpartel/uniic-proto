package uniic.parsers
import org.scalatest.matchers.{MatchResult, Matcher}
import scala.util.parsing.input.{Position, Positional}

trait PositionalTestUtils {

  /** Matches the position of a `Positional`. */
  def beAt[T <: Positional](expected: Position): Matcher[T] =
    new Matcher[T] {
      def at(p: Position): String = at(p.line, p.column)
      def at(line: Int, column: Int) = "at line " + line + ", column " + column
      def apply(x: T) =
        MatchResult(
          x.pos.line == expected.line && x.pos.column == expected.column,
          x + " was " + at(x.pos) + ", expected " + at(expected.line, expected.column),
          x + " was " + at(expected.line, expected.column)
        )
    }

  /** Matches the line and column of a `Positional`. */
  def beAt[T <: Positional](expectedLine: Int, expectedColumn: Int): Matcher[T] =
    beAt(new MockPosition(expectedLine, expectedColumn))


  case class MockPosition(override val line: Int, override val column: Int) extends Position {
    override def lineContents: String = ""
    override def longString = toString
  }
}