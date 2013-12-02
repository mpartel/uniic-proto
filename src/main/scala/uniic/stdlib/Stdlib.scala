package uniic.stdlib

import uniic.module.CompiledModule
import uniic.grs.Grs
import uniic.types.TypeBorrowingRemoval
import uniic.misc.VarStream
import uniic.module.Stage

trait Stdlib extends LibraryProvider with BaseLibrary with IntArrays {
  def moduleName = "Stdlib"
  lazy val asModule: CompiledModule = {
    val definitions = symTab.toSeq.sortBy(_._1).map {
      case (name, Stage.SymTabEntry(value, bts, nbts)) => {
        CompiledModule.Entry(name, Grs(value.toNode), bts, nbts)
      }
    }
    new CompiledModule(moduleName, definitions)
  }
}

object Stdlib extends Stdlib
