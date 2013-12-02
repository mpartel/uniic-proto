package uniic.misc

import scala.collection.mutable
import uniic.flow.FlowVar

/** Generates unique indexes for names, e.g. x1, x2, .... */
class NameIndexer(reserved: Set[(String, Int)] = Set.empty) {
  private val nameToIndex = mutable.Map.empty[String, Int]

  def next(baseName: String): String = {
    baseName + nextIndex(baseName)
  }

  def nextIndex(baseName: String): Int = {
    var result = 0
    do {
      result = nameToIndex.getOrElse(baseName, 0)
      nameToIndex.update(baseName, result + 1)
    } while (reserved(baseName, result))
    result
  }

  def nextFlowVar(baseName: String): FlowVar = {
    FlowVar(baseName, nextIndex(baseName))
  }

  def lastIndex(baseName: String): Option[Int] = nameToIndex.get(baseName).map(_ - 1)

  def allNames: Iterable[String] = nameToIndex.keys
}
