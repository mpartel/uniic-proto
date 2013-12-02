package uniic.module

import uniic.misc.CollectionUtil
import uniic.types.TypeSchema
import uniic.grs._
import CompiledModule.Entry

class CompiledModule(
  val name: String,
  val definedEntries: Seq[Entry]
) {
  CollectionUtil.findDuplicate(definedEntries.map(_.name)).foreach { dup =>
    throw new IllegalArgumentException("Duplicate module entry: " + dup)
  }

  val entryMap: Map[String, Grs] = definedEntries.map { case Entry(n, g, _, _) => n -> g }.toMap
  val borrowingTypeMap: Map[String, TypeSchema] = definedEntries.map { case Entry(n, _, t, _) => n -> t }.toMap
  val nonborrowingTypeMap: Map[String, TypeSchema] = definedEntries.map { case Entry(n, _, _, t) => n -> t }.toMap
}

object CompiledModule {
  case class Entry(
    name: String,
    grs: Grs,
    borrowingType: TypeSchema,
    nonborrowingType: TypeSchema
  )
}
