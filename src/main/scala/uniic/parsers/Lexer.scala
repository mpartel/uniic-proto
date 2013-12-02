package uniic.parsers

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.lexical.Lexical
import uniic.misc.Unicode
import uniic.types.MBorrowed

/** The tokenizer. */
private[parsers] class Lexer extends Lexical with LangTokens {
  import Lexer._

  def token = (
      specialOperator
    | intLiteral
    | quotedString('\'')
    | quotedString('\"')
    | identifierOrKeyword
    | paren
    | eof
    | operator
    | errors
  )

  def specialOperator = (
      ';' ^^^ { Kw(";") }
    | '_' ^^^ { Kw("_") }
    | '@' ^^^ { Kw("@") }
    | '?' ^^^ { Kw("?") }
    | '|' ^^^ { Kw("|") }
    | '\\' ~ '/' ^^^ { Kw("\\/") }
    | '/' ~ '\\' ^^^ { Kw("/\\") }
    | rep1('*') ^^ { x => Kw(x.foldLeft("")(_ + _)) }
    | '-' ~ '(' ^^^ { Kw("-(") }
    | ')' ~ '>' ^^^ { Kw(")>") }
    | ':' ~ '=' ^^^ { Kw(":=") }
    | ':' ^^^ { Kw(":") }
    | '.' ^^^ { Kw(".") }
    | ',' ^^^ { Kw(",") }
    | '^' ^^^ { Kw("^") }
    | '\\' ^^^ { Kw("\\") }
    | Unicode.lambdaSymbol ^^^ { Kw(""+Unicode.lambdaSymbol) }
  )

  def identifierOrKeyword = {
    def processIdent(name: String) = {
      if (keywords contains name) Keyword(name) else Ident(name)
    }

    (
      (identInitialChar ~ rep(identLaterChar)) ^^
        { case x ~ xs => processIdent(x :: xs mkString "") }
      | (elem(':') ~ identInitialChar ~ rep(identLaterChar)) >>
        { case x1 ~ x2 ~ xs =>
          val s = x1 :: x2 :: xs mkString ""
          if (keywords.contains(s)) {
            success(Keyword(s))
          } else {
            failure("Not a keyword: " + s)
          }
        }
    )
  }

  def identInitialChar = letter | elem('_') | elem('$')
  def identLaterChar = identInitialChar | digit | unicodeSubscript

  def unicodeSubscript = elem("unicode subscript", Unicode.subscriptDigits.contains(_))

  def keywords = Set(
    "borrowed", "immutable", "var",
    "forall", "Bool", "Int",
    "true", "false",
    "let", "letrec", "and", "in",
    "if", "then", "else",
    "match", "case", "_",
    "return", "while", "do",
    "module", "def",
    ":quit", ":type", ":typeimp", ":tr"
  )

  def intLiteral = (
    opt(elem('-')) ~ rep1(digit) ^^
      { case Some(minus) ~ ds => IntLit(minus :: ds mkString "")
        case None ~ ds => IntLit(ds mkString "") }
  )

  def quotedString(quote: Char) = (
    quote ~ rep(strLitChar(quote)) ~ quote ^^
      { case _ ~ chars ~ _ => StringLit(chars mkString "") }
  )

  def strLitChar(quote: Char) = (
      '\\' ~ '\'' ^^^ '\''
    | '\\' ~ '\"' ^^^ '\"'
    | '\\' ~ 'n' ^^^ '\n'
    | '\\' ~ 't' ^^^ '\t'
    | chrExcept(quote, '\n', EofCh)
  )

  def paren = (
      '(' ^^^ LParen
    | ')' ^^^ RParen
    | '[' ^^^ LBracket
    | ']' ^^^ RBracket
    | '{' ^^^ LBrace
    | '}' ^^^ RBrace
  )

  def operator = (
    rep1(operatorChar) ^^
      { case xs => Keyword(xs mkString "") }
  )

  def operatorChar = elem("operator character",  {
    c => !c.isLetterOrDigit &&
         !c.isWhitespace &&
         !c.isControl &&
         !"([{}]),;'\"".contains("" + c)
  })

  def eof = EofCh ^^^ EOF

  def errors = (
      '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | failure("illegal character")
  )

  def whitespace = rep(
      whitespaceChar
    | '#' ~ rep(chrExcept(EofCh, '\n'))
  )

}

object Lexer {
  def isIdentOperator(op: String) = {
    val initials = Seq("+", "-", "*", "/", "<", ">", "=", "%")
    initials.exists(op.startsWith(_))
  }
}
