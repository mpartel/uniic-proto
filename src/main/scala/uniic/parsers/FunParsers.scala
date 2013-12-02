package uniic.parsers
import uniic.fun._
import uniic.misc.Unicode
import uniic.types._

private[parsers] trait FunParsers extends CommonParsers with InfixOperators with TypeExprParsers {

  lazy val funExpr: PP[FunExpr] = lowestPrecedence

  type InfixOperand = FunExpr

  protected def makeInfixExpression(left: InfixOperand, op: String, right: InfixOperand) = FunApply(FunVar(op), Seq(left, right))

  protected lazy val highestPrecedence: PP[FunExpr] = (
      application
    | ifExpr
    | matchExpr
    | lambda
    | letExpr
    | unitConst
    | boolConst
    | intConst
    | funVar
    | parenthesizedExpr
  )

  private lazy val application: PP[FunApply] = (
    funExpr ~ parenthesizedList(funExpr) ^^
      { case fun ~ args => FunApply(fun, args) }
  )

  private lazy val parenthesizedExpr: PP[FunExpr] = (
    parenthesizedList(funExpr) ^^ {
      case List() => FunUnit
      case List(e) => e
      case exprs => FunTupleExpr(exprs)
    }
  )

  private lazy val lambda: PP[FunLambda] = (
    ("\\" | ""+Unicode.lambdaSymbol) ~> repsep(lambdaParam, ",") ~ "." ~ funExpr ^^
      { case params ~ _ ~ body => FunLambda(params, body) }
  )

  private lazy val lambdaParam: PP[(String, TypeAnnotation)] = (
    ident ~ opt(":" ~> typeTerm) ^^
      { case name ~ te => (name, te.map(TypeExprTypeAnnotation).getOrElse(MissingTypeAnnotation)) }
  )

  private lazy val letExpr: PP[FunExpr] = (
    "let" ~> rep1sep(letBinding, "and") ~ "in" ~ funExpr ^^
      { case bindings ~ _ ~ body => FunLet(bindings, body) }
  )

  private lazy val letBinding: PP[(String, FunExpr)] = (
    ident ~ "=" ~ funExpr ^^ { case v ~ _ ~ expr => (v, expr) }
  )

  private lazy val ifExpr: PP[FunIfThenElse] = (
    "if" ~ funExpr ~ "then" ~ funExpr ~ "else" ~ funExpr ^^
      { case _ ~ e1 ~ _ ~ e2 ~ _ ~ e3 => FunIfThenElse(e1, e2, e3) }
  )

  private lazy val matchExpr: PP[FunMatch] = (
    funExpr ~ "match" ~ lexical.LBrace ~ rep1(matchCase) <~ lexical.RBrace ^^
      { case head ~ _ ~ _ ~ cases => FunMatch(head, cases) }
  )

  private lazy val matchCase: PP[FunCase] = (
    "case" ~> pattern ~ "=>" ~ funExpr <~ opt(";") ^^ { case p ~ _ ~ e => FunCase(p, e) }
  )

  private lazy val pattern: PP[Pattern] = (
      "_" ^^^ { PatUnused }
    | unitConst ^^^ { PatUnit }
    | boolConst ^^ { case FunBool(value) => PatBool(value) }
    | intConst ^^ { case FunInt(value) => PatInt(value) }
    | parenthesizedList(pattern) ^^ { PatTuple(_) }
    | ident ^^ { PatVar(_) }
  )

  lazy val unitConst: PP[FunUnit.type] = (
    lexical.LParen ~ lexical.RParen ^^^ { FunUnit }
  )

  lazy val boolConst: PP[FunBool] = (
      "true" ^^^ { FunBool(true) }
    | "false" ^^^ { FunBool(false) }
  )

  lazy val intConst: PP[FunInt] = (
    elem("integer literal", _.isInstanceOf[lexical.IntLit]) ^^ { nl => FunInt(nl.chars.toInt) }
  )

  lazy val funVar: PP[FunVar] = (
      ident ^^ { FunVar(_) }
    | identOperator ^^ { FunVar(_) }
  )
}