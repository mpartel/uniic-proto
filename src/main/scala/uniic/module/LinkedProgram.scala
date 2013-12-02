package uniic.module

import uniic.types.TypeSchema
import uniic.grs._

/** A set of modules linked into on big Grs with no remaining GrsExternals. */
class LinkedProgram(
  val grs: Grs,
  val borrowingTypeMap: Map[String, TypeSchema],
  val nonborrowingTypeMap: Map[String, TypeSchema]
) {
  val moduleNode = grs.rootNode.soleChild.downcast[GrsModule]
  require(borrowingTypeMap.keys.forall(k => moduleNode.value.entryNames.contains(k)))

  val nodeMap: Map[String, GrsNode] = {
    moduleNode.value.entryNames.zip(moduleNode.children).toMap
  }

  def stripUnusedSymbols: LinkedProgram = {
    val usedNodes: Set[GrsNode] = moduleNode.children.toSet.flatMap { n: GrsNode =>
      grs.descendantsOf(n, true).filter(moduleNode.children.contains)
    } ++ {
      val mainIndex = moduleNode.value.entryNames.indexOf("main")
      if (mainIndex != -1) Some(moduleNode.children(mainIndex)) else None
    }

    val newEntries = moduleNode.value.entryNames.zip(moduleNode.children).filter {
      case (_, node) => usedNodes.contains(node)
    }

    val newBorrowingTypeMap = borrowingTypeMap.filterKeys(k => newEntries.exists(_._1 == k))
    val newNonorrowingTypeMap = nonborrowingTypeMap.filterKeys(k => newEntries.exists(_._1 == k))

    val newGrs = Grs(GrsModule.makeNode(newEntries))

    new LinkedProgram(newGrs, newBorrowingTypeMap, newNonorrowingTypeMap)
  }
}
