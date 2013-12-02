package uniic.flow

import uniic.types.TFunParam
import uniic.types.Type

sealed trait FlowToplevel

case class FlowFunDef(params: Seq[(String, TFunParam)], returnType: Option[Type], body: FlowGraph) extends FlowToplevel {
  override def toString = {
    params.mkString("(", ", ", ")") + " { ... }"
  }
}
