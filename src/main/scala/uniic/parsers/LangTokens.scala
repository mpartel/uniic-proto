package uniic.parsers

import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.Positional

/** A bunch of tokens. */
private[parsers] trait LangTokens extends StdTokens {
  // Shorthands

  type Ident = Identifier
  type Kw = Keyword
  val Ident = Identifier
  val Kw = Keyword

  case class IntLit(chars: String) extends Token {
    override def toString = chars
  }

  /** Parenthesis tokens are objects inheriting this class */
  abstract class ParenthesisToken(parenChar: Char) extends Token { // TODO: why aren't they just Keywords?
    override val chars = "" + parenChar
    override def toString = chars
  }

  object LParen extends ParenthesisToken('(')
  object RParen extends ParenthesisToken(')')
  object LBracket extends ParenthesisToken('[')
  object RBracket extends ParenthesisToken(']')
  object LBrace extends ParenthesisToken('{')
  object RBrace extends ParenthesisToken('}')

  /** A token that carries its position.
    *
    * An implicit conversion may be imported to wrap any token into a `PositionalToken`.
    * A parser whose result type is `PositionalToken` (or any `Positional`)
    * will set the positions automatically. */
  case class PositionalToken(val tok: Token) extends Positional

  object PositionalToken {
    object ImplicitConversion {
      implicit def convertFromToken(tok: Token): PositionalToken = PositionalToken(tok)
    }
  }
}
