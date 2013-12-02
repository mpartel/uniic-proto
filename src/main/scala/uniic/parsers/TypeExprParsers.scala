package uniic.parsers
import uniic.types._

private[parsers] trait TypeExprParsers extends CommonParsers {
  lazy val typeExpr: PP[TypeExpr] = (
      funType
    | typeTerm
  )

  lazy val typeTerm: PP[TypeExpr] = (
      typeSchema
    | typeAtom <~ "*" ^^ { TEAttribution(_, TEUniq) }
    | typeAtom <~ "+" ^^ { TEAttribution(_, TENonUniq) }
    | typeAtom ~ "^" ~ typeAttr ^^ { case t ~ _ ~ a => TEAttribution(t, a) }
    | typeAtom
  )

  private lazy val typeAtom: PP[TypeExpr] = (
      ident ^^ { TEVar(_) }
    | "Bool" ^^^ { TEVar("Bool") } // These could be made non-keywords
    | "Int" ^^^ { TEVar("Int") }
    | lexical.LParen ~ lexical.RParen ^^^ TEUnit
    | lexical.LParen ~> rep2sep(typeExpr, ",") <~ lexical.RParen ^^ { TETuple(_) }
    | lexical.LParen ~> typeExpr <~ lexical.RParen
  )

  private lazy val typeSchema: PP[TESchema] = (
    "forall" ~> commit(rep1(typeSchemaParam) ~ "." ~ typeExpr) ^^ ({
      case vars ~ _ ~ body => TESchema(vars, body)
    })
  )

  private lazy val typeSchemaParam: PP[(String, Kind)] = (
      ident <~ ":" <~ specificIdent("T") ^^ { (_, KBaseType) }
    | ident <~ ":" <~ specificIdent("A") ^^ { (_, KTypeAttr) }
  )

  private def specificIdent(name: String): PP[String] = (
    ident >> { s => if (s == name) success(s) else failure(name + " expected") }
  )

  private lazy val funType: PP[TypeExpr] = (
    simplerChainr2(funParamList, arrowCombiner, typeTerm)
  )

  private lazy val funParamList: PP[List[TEFunParam]] = (
      lexical.LParen ~> repsep(funParamTypeExpr, ",") <~ lexical.RParen
    | funParamTypeExpr ^^ { List(_) }
  )

  lazy val funParamTypeExpr: PP[TEFunParam] = (
    funParamMode ~ typeTerm ^^
      { case mode ~ paramType => TEFunParam(paramType, mode) }
  )

  lazy val funParamMode: PP[ParamMode] = (
      MBorrowed.symbol ^^^ { MBorrowed }
    | success(MNone)
  )

  private val defaultClosureAttr = TENonUniq

  private lazy val arrow: PP[(Option[TypeExpr], TypeExpr)] = (
      "->" ^^^ { (None, defaultClosureAttr) }
    | "-+>" ^^^ { (Some(TENonUniq), defaultClosureAttr) }
    | "-*>" ^^^ { (Some(TEUniq), defaultClosureAttr) }
    | "-" ~> typeAttr <~  ">" ^^ { a => (Some(a), defaultClosureAttr) }
    | "-(" ~> arrowContents <~  ")>"
    | failure("arrow expected")
  )

  private lazy val arrowContents: PP[(Option[TypeExpr], TypeExpr)] = (
    opt(typeAttr) ~ opt("|" ~> typeAttr) ^^
      { case mainAttr ~ closureAttr => (mainAttr, closureAttr.getOrElse(defaultClosureAttr)) }
  )

  private lazy val arrowCombiner: PP[(List[TEFunParam], TypeExpr) => TypeExpr] = {
    arrow ^^ {
      case (Some(te), ca) => { (params: List[TEFunParam], right: TypeExpr) => TEFun(params, ca, right).withAttr(te) }
      case (None, ca) => { (params: List[TEFunParam], right: TypeExpr) => TEFun(params, ca, right) }
    }
  }

  private lazy val typeAttr: PP[TypeExpr] = (
      typeAttrLiteral ~ """/\""" ~ typeAttr ^^ { case l ~ _ ~ r => TEAnd(l, r) }
    | typeAttrLiteral ~ """\/""" ~ typeAttr ^^ { case l ~ _ ~ r => TEOr(l, r) }
    | typeAttrLiteral
    | success(TENonUniq)
  )

  private lazy val typeAttrLiteral: PP[TypeExpr] = (
      lexical.LParen ~> typeAttr <~ lexical.RParen
    | "~" ~> typeAttrLiteral ^^ { TENot(_) }
    | "*" ^^^ { TEUniq }
    | "+" ^^^ { TENonUniq }
    | ident ^^ { TEVar(_) }
  )
}