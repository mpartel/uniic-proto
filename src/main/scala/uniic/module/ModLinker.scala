package uniic.module

import uniic.misc._
import uniic.grs._
import uniic.types.TypeSchema

trait ModLinker extends ExceptionContexts {
  import CompiledModule.Entry

  def link(compiledModules: Seq[CompiledModule]): LinkedProgram = {
    val definitionMap: Map[String, (GrsNode, TypeSchema, TypeSchema)] = {
      compiledModules.flatMap { mod =>
        mod.definedEntries.map { case Entry(name, grs, bts, nbts) => (name, (grs.rootNode.soleChild, bts, nbts)) }.toMap
      }
    }.toMap

    val unlinkedGrs = Grs(GrsModule.makeNode(
      definitionMap.toSeq.map { case (name, (node, _, _)) => (name, node) }.sortBy(_._1)
    ))

    val redirectMap: Map[GrsNode, GrsNode] = unlinkedGrs.allNodes.flatMap {
      case extNode@GrsNode(GrsExternal(name)) => {
        val defNode = definitionMap.getOrElse(
          name,
          throw new CompilerException(s"No definition found for $name during linking")
        )._1
        Some(extNode -> defNode)
      }
      case _ => None
    }.toMap

    val linkedGrs = unlinkedGrs.withRedirects(redirectMap)

    val borrowingTypeMap = definitionMap.mapValues(_._2)
    val nonborrowingTypeMap = definitionMap.mapValues(_._3)

    new LinkedProgram(linkedGrs, borrowingTypeMap, nonborrowingTypeMap)
  }

  private def findAllExternalNodes(mod: CompiledModule): Seq[(String, GrsNodeOf[GrsExternal])] = {
    mod.definedEntries.flatMap { entry =>
      entry.grs.allNodes.flatMap {
        case node@GrsNode(GrsExternal(name)) => Some(name -> node.downcast[GrsExternal])
        case _ => None
      }
    }
  }
}

object ModLinker extends ModLinker with HasDebugVersion[ModLinker] {
  trait WithDebug extends ModLinker with DebugContexts
  val withDebug = new WithDebug {}
}
