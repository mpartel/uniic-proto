package uniic.module

import uniic.misc.CollectionUtil
import uniic.types.TypeExpr
import uniic.fun.FunExpr
import uniic.imp.ImpToplevel

sealed trait UncompiledModEntry
case class UncompiledModImpEntry(impToplevel: ImpToplevel) extends UncompiledModEntry
case class UncompiledModFunEntry(funExpr: FunExpr) extends UncompiledModEntry

class UncompiledModule(
  val name: String,
  val entries: Seq[(String, UncompiledModEntry, TypeExpr)]
) {
  CollectionUtil.findDuplicate(entries.map(_._1)).foreach { dup =>
    throw new IllegalArgumentException("Duplicate module entry: ")
  }
}
