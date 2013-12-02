package uniic.imp

import uniic.types.TEFunParam
import uniic.types.TypeExpr

sealed trait ImpToplevel

case class ImpFunDef(params: Seq[(String, TEFunParam)], expectedReturnType: Option[TypeExpr], body: ImpStmt) extends ImpToplevel
