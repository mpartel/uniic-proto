package uniic.parsers

import uniic.imp._
import uniic.fun._
import uniic.module._
import uniic.types.TEFunParam
import scala.util.parsing.combinator.Parsers
import uniic.types.TypeExpr
import uniic.types.TEFun

private[parsers] trait ModuleParsers extends CommonParsers {

  val moduleInfixOperators: Map[String, (Int, Associativity)]

  // We can't inherit both ImpParsers and FunParsers because they both extends InfixOperators differently.
  // The following ugly hax allows us to still use both.

  trait Subparsers extends CommonParsers {
    self: InfixOperators =>
    override val lexical: ModuleParsers.this.lexical.type = ModuleParsers.this.lexical
    override val infixOperators = moduleInfixOperators
  }

  object ModuleImpParsers extends ImpParsers with Subparsers
  object ModuleFunParsers extends FunParsers with Subparsers

  def useSubparser[T](ps: Subparsers)(p: ps.Parser[T]): Parser[T] = {
    new Parser[T] {
      def apply(in: Input) = p(in) match {
        case ps.Success(r, n) => Success(r, n)
        case ps.Failure(r, n) => Failure(r, n)
        case ps.Error(r, n) => Error(r, n)
      }
    }
  }
  implicit def useImpParser[T] = useSubparser[T](ModuleImpParsers) _
  implicit def useFunParser[T] = useSubparser[T](ModuleFunParsers) _


  lazy val module: PP[UncompiledModule] = (
    "module" ~> ident ~ rep(moduleEntry) ^^
      { case name ~ es => new UncompiledModule(name, es) }
  )

  lazy val moduleEntry: PP[(String, UncompiledModEntry, TypeExpr)] = (
      impToplevel ^^ { case (n, e, te) => (n, UncompiledModImpEntry(e), te) }
    | funToplevel ^^ { case (n, e, te) => (n, UncompiledModFunEntry(e), te) }
  )

  lazy val impToplevel: PP[(String, ImpToplevel, TypeExpr)] = (
    "def" ~> commit(ident ~ parenthesizedList(defParam) ~ (":" ~> commit(typeExpr)) ~ ModuleImpParsers.blockStmt) ^^
      { case name ~ params ~ returnTe ~ body => (name, ImpFunDef(params, Some(returnTe), body), mkTEFun(params, returnTe)) }
  )

  private lazy val defParam: PP[(String, TEFunParam)] = (
    ident ~ (":" ~> commit(ModuleFunParsers.funParamTypeExpr)) ^^
      { case name ~ te => (name, te) }
  )

  lazy val funToplevel: PP[(String, FunExpr, TypeExpr)] = (
    "let" ~> commit(ident ~ (":" ~> commit(typeExpr))) ~ ("=" ~> commit(ModuleFunParsers.funExpr)) ^^
      { case name ~ te ~ e => (name, e, te) }
  )

  private lazy val typeExpr = ModuleFunParsers.typeExpr

  private def mkTEFun(params: Seq[(String, TEFunParam)], returnTe: TypeExpr) = {
    TEFun(params.map(_._2), returnTe)
  }
}
