package uniic.parsers
import uniic.fun.FunExpr
import uniic.imp.ImpStmt
import uniic.types.TypeExpr
import uniic.misc.CompilerException
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import uniic.module.UncompiledModule

/** Facade for our parsers */
object LangParsers {
  type OperatorSet = Map[String, (Int, Associativity)]
  object OperatorSet {
    val empty = Map.empty[String, (Int, Associativity)]
  }

  def parseModule(s: String, operatorSet: OperatorSet): Either[String, UncompiledModule] = {
    val parsers = new ModuleParsers {
      override val moduleInfixOperators = operatorSet
    }
    val scanner = new parsers.lexical.Scanner(new CharSequenceReader(s))
    parsers.phrase(parsers.module)(scanner) match {
      case parsers.Success(r, _) => Right(r)
      case parsers.NoSuccess(err, _) => Left(err + " at " + scanner.pos)
    }
  }

  def parseImpStmt(s: String, operatorSet: OperatorSet): Either[String, ImpStmt] = {
    val parsers = new ImpParsers {
      override val infixOperators = operatorSet
    }
    val scanner = new parsers.lexical.Scanner(new CharSequenceReader(s))
    parsers.phrase(parsers.impStmt)(scanner) match {
      case parsers.Success(r, _) => Right(r)
      case parsers.NoSuccess(err, _) => Left(err + " at " + scanner.pos)
    }
  }

  def parseFunExpr(s: String, operatorSet: OperatorSet): Either[String, FunExpr] = {
    val parsers = new FunParsers {
      override val infixOperators = operatorSet
    }
    val scanner = new parsers.lexical.Scanner(new CharSequenceReader(s))
    parsers.phrase(parsers.funExpr)(scanner) match {
      case parsers.Success(r, _) => Right(r)
      case parsers.NoSuccess(err, _) => Left(err + " at " + scanner.pos)
    }
  }

  def parseTypeExpr(s: String): Either[String, TypeExpr] = {
    val parsers = new TypeExprParsers {}
    val scanner = new parsers.lexical.Scanner(new CharSequenceReader(s))
    parsers.phrase(parsers.typeExpr)(scanner) match {
      case parsers.Success(r, _) => Right(r)
      case parsers.NoSuccess(err, _) => Left(err + " at " + scanner.pos)
    }
  }

  def parseModuleOrThrow(s: String, operatorSet: OperatorSet) = orThrow(parseModule(s, operatorSet))
  def parseFunExprOrThrow(s: String, operatorSet: OperatorSet) = orThrow(parseFunExpr(s, operatorSet))
  def parseImpStmtOrThrow(s: String, operatorSet: OperatorSet) = orThrow(parseImpStmt(s, operatorSet))
  def parseTypeExprOrThrow(s: String) = orThrow(parseTypeExpr(s))

  private def orThrow[T](either: Either[String, T]): T = {
    either match {
      case Left(err) => throw new CompilerException(err)
      case Right(x) => x
    }
  }
}