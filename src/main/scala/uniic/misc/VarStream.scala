package uniic.misc

import uniic.types.{TVar, AVar, Type}
import uniic.flow.FlowVar

object VarStream {
  def forTVars(existing: Set[String]): Stream[TVar] = {
    prefixedAndSubscripted(existing, Unicode.tauSymbol).map(TVar)
  }

  def forAVars(existing: Set[String]): Stream[AVar] = {
    prefixedAndSubscripted(existing, Unicode.upsilonSymbol).map(AVar)
  }

  def forTypesWithAttrs(existing: Set[String]): Stream[Type] = {
    forTVars(existing).zip(forAVars(existing)).map { case (t, a) => Type(t, a) }
  }

  def forFlowVars(prefix: String, existing: Set[FlowVar]): Stream[FlowVar] = {
    Stream.from(0).map { n => FlowVar(prefix, n) }.filterNot(existing)
  }

  def prefixed(existing: Set[String], prefix: String): Stream[String] = {
    Stream.from(1).map { n =>
      "" + prefix
    }.filterNot(existing)
  }

  def prefixedAndSubscripted(existing: Set[String], prefix: Char): Stream[String] = {
    Stream.from(1).map { n =>
      "" + prefix + Unicode.subscript(n)
    }.filterNot(existing)
  }
}
