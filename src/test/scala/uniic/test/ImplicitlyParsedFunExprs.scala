package uniic.test

import uniic.fun.FunExpr
import uniic.fun.analysis.FunTree
import uniic.parsers.LangParsers
import uniic.stdlib.Stdlib

trait ImplicitlyParsedFunExprs {
  implicit def parseFunExpr(s: String): FunExpr = {
    LangParsers.parseFunExprOrThrow(s, Stdlib.defaultOperatorSet)
  }
  implicit def parseFunTree(s: String): FunTree = {
    new FunTree(parseFunExpr(s))
  }
}
