package uniic.flow

import uniic.misc.Unicode

case class FlowVar(baseName: String, version: Int) extends Ordered[FlowVar] {
  def encodedName = FlowVar.encodeName(baseName, version)

  def compare(that: FlowVar) = FlowVar.defaultOrdering.compare(this, that)

  def mapBaseName(f: String => String) = FlowVar(f(baseName), version)

  override def toString = {
    if (version == 0) baseName
    else baseName + Unicode.subscript(version)
  }

  def toTuple = (baseName, version)
}

object FlowVar {
  def encodeName(baseName: String, version: Int): String = {
    if (version == 0) baseName
    else baseName + "$" + version
  }
  def decodeName(encodedName: String): FlowVar = {
    encodedName match {
      case Pattern(baseName, version) => FlowVar(baseName, version.toInt)
      case _ => FlowVar(encodedName, 0)
    }
  }
  private val Pattern = "^(.+)\\$(\\d+)$".r
  private val defaultOrdering = Ordering[(String, Int)].on { v: FlowVar => (v.baseName, v.version) }
}