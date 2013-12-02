package uniic.parsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.Positional

/** Superclass of our parsers classes. */
private[parsers] trait CommonParsers extends StdTokenParsers with PackratParsers {
  override type Tokens = LangTokens
  override val lexical = new Lexer()

  import lexical.PositionalToken

  type PP[T] = PackratParser[T]

  def withCheck[T](p: Parser[T])(cond: T => Boolean)(failureMsg: T => String): Parser[T] = {
    new Parser[T] {
      def apply(in: Input) = {
        p(in) match {
          case success@Success(result, next) => {
            if (cond(result)) {
              success
            } else {
              Failure(failureMsg(result), next)
            }
          }
          case result => result
        }
      }
    }
  }

  def simplerChainr1[T](p: => Parser[T], q: => Parser[(T, T) => T]): Parser[T] = {
    rep(p ~ q) ~ p ^^ {
      case xs ~ x => xs.foldRight(x){(_, _) match {case (a ~ f, b) => f(a, b)}}
    }
  }

  def simplerChainr1[T, U](p: => Parser[T], q: => Parser[(T, U) => U], first: Parser[U]): Parser[U] = {
    rep(p ~ q) ~ first ^^ {
      case xs ~ x => xs.foldRight(x){(_, _) match {case (a ~ f, b) => f(a, b)}}
    }
  }

  def simplerChainr2[T, U](p: => Parser[T], q: => Parser[(T, U) => U], first: Parser[U]): Parser[U] = {
    rep1(p ~ q) ~ first ^^ {
      case xs ~ x => xs.foldRight(x){(_, _) match {case (a ~ f, b) => f(a, b)}}
    }
  }

  def parenthesizedList[T](p: PP[T]): PP[List[T]] = (
    lexical.LParen ~> repsep(p, ",") <~ lexical.RParen
  )

  def optionalParens[T](p: PP[T]): PP[T] = (
    lexical.LParen ~> p <~ lexical.RParen | p
  )

  // Add a more specific implicit conversion for parsers of positional elements.
  implicit def mkPositionalPackrat[T <: Positional](p: => super.Parser[T])(implicit manifest: Manifest[T]): PackratParser[T] = {
    super.parser2packrat(positioned(p))
  }

  implicit def positionedTok(p: Parser[lexical.Token]): PP[PositionalToken] =
    mkPositionalPackrat(p ^^ { PositionalToken(_) })

  def rep2sep[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] = {
    p ~ q ~ rep1sep(p, q) ^^ { case x ~ _ ~ xs => x :: xs }
  }

  implicit def addDebugToParser[T](p: Parser[T]) = new {
    def debug(label: String): Parser[T] = log(p)(label) ^^ { e => println(label + " => " + e); e }
  }
}