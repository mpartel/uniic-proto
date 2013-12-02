package uniic.parsers
import org.scalatest.matchers.{ClassicMatchers, MatchResult, Matcher}
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, CharSequenceReader}

trait ParsersTestBase extends ClassicMatchers with PositionalTestUtils {
  val parsers: Parsers
  import parsers._

  def mkScanner(reader: Reader[Char]): parsers.Input

  /** Enables the syntax `"foo" parsedWith p should parseAs (Something())`. */
  def parseAs[T](right: T): Matcher[ParseResult[T]] = be ('successful) and inTheResult (be (right))

  /** A conversion necessary when asserting that a parser for T produces a result in U <: T. */
  implicit def upcastParseResultMatcher[T, U <: T](a: Matcher[ParseResult[U]]): Matcher[ParseResult[T]] =
    a.asInstanceOf[Matcher[ParseResult[T]]]

  /** Enables the syntax `"foo" parsedWith p`. */
  implicit def convertStringToParseable(str: String): Parseable = new Parseable(str)

  /** Enables the syntax `"foo" parsedWith p`. */
  class Parseable(str: String) {
    def parsedWith[T](parser: Parser[T]): ParseResult[T] = {
      var reader = mkScanner(new CharSequenceReader(str))
      val fullParser = phrase(parser)
      fullParser(reader)
    }
  }

  /** Enables the syntax `"foo" parsedWith p should inTheResult (haveSomeProperty)`. */
  def inTheResult[T](matcher: Matcher[T]): Matcher[ParseResult[T]] =
    new Matcher[ParseResult[T]] {
      def apply(pr: ParseResult[T]) =
        if (pr.successful) matcher(pr.get)
        else MatchResult(
          false,
          "The parse was not successful",
          "The parse was successful"
        )
    }
}