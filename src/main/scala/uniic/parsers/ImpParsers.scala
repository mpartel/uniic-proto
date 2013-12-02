package uniic.parsers
import uniic.imp._
import uniic.types.TEFunParam
import uniic.types.{ParamMode, MBorrowed, MNone}

private[parsers] trait ImpParsers extends CommonParsers with InfixOperators with TypeExprParsers {
  lazy val impStmt: PP[ImpStmt] = (
      returnStmt <~ ";"
    | assignmentStmt <~ ";"
    | ifStmt
    | whileStmt
    | blockStmt
    | impExpr <~ ";"
  )

  private lazy val returnStmt: PP[ImpReturn] = (
    "return" ~> commit(impExpr ^^ { ImpReturn(_) })
  )

  private lazy val assignmentStmt: PP[ImpStmt] = (
    (optionalParens(rep1sep(impVar, ",")) <~ ":=") ~ commit(impExpr) ^^ {
      case Seq(v) ~ e => ImpSet(v, e)
      case vs ~ e => ImpSplitTuple(vs, e)
    }
  )

  private lazy val ifStmt: PP[ImpIf] = (
    "if" ~> commit(impExpr) ~ ("then" ~> commit(blockStmt)) ~ opt("else" ~> commit(blockStmt)) ^^ {
      case c ~ t ~ Some(e) => ImpIf(c, t, e)
      case c ~ t ~ None => ImpIf(c, t, ImpBlock(Seq.empty))
    }
  )

  private lazy val whileStmt: PP[ImpWhile] = (
    "while" ~> commit(impExpr) ~ "do" ~ commit(blockStmt) ^^
      { case c ~ _ ~ b => ImpWhile(c, b) }
  )

  lazy val blockStmt: PP[ImpStmt] = (
    lexical.LBrace ~> rep(impStmt) <~ lexical.RBrace ^^ {
      case Seq(s) => s
      case ss => ImpBlock(ss)
    }
  )


  lazy val impExpr: PP[ImpExpr] = lowestPrecedence

  type InfixOperand = ImpExpr

  protected def makeInfixExpression(left: InfixOperand, op: String, right: InfixOperand) = {
    ImpCall(ImpVar(op), Seq(ImpNormalArg(left), ImpNormalArg(right)))
  }

  protected lazy val highestPrecedence: PP[ImpExpr] = (
      parenthesizedImpExprs
    | callExpr
    | impConst
    | impVar
  )

  private lazy val callExpr: PP[ImpExpr] = (
      (impArg <~ ".") ~ commit(impVar) ~ parenthesizedList(impArg) ^^
      { case (arg1 ~ f ~ args) => ImpCall(f, arg1 +: args) }
    | impExpr ~ parenthesizedList(impArg) ^^
      { case e ~ args => ImpCall(e, args) }
  )

  private lazy val parenthesizedImpExprs: PP[ImpExpr] = (
    parenthesizedList(impExpr) ^^ {
      case Seq() => ImpUnit
      case Seq(e) => e
      case args => ImpMakeTuple(args)
    }
  )

  private lazy val impConst: PP[ImpConst] = (
    impBoolConst | impIntConst
  )

  lazy val impBoolConst: PP[ImpBool] = (
      "true" ^^^ { ImpBool(true) }
    | "false" ^^^ { ImpBool(false) }
  )

  lazy val impIntConst: PP[ImpInt] = (
    elem("integer literal", _.isInstanceOf[lexical.IntLit]) ^^ { nl => ImpInt(nl.chars.toInt) }
  )

  private lazy val impVar: PP[ImpVar] = ident ^^ { ImpVar(_) }

  private lazy val impArg: PP[ImpArg] = (
      (MBorrowed.symbol ~> commit(impVar)) ^^ { ImpBorrowedArg(_) }
    | impExpr ^^ { ImpNormalArg(_) }
  )
}
