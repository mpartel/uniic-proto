package uniic.integration

import uniic.module._
import uniic.grs._
import uniic.parsers.LangParsers
import uniic.types.TypeSchema
import uniic.types.TypeSchemaAssertions
import uniic.stdlib.Stdlib
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait ModuleLevelItBase extends IntegrationTestBase with TypeSchemaAssertions {
  implicit class ImperativeProgram(val code: String) {
    val compiledModule = compile(code)
    val linkedProgram = link(Stdlib.asModule, compiledModule)

    def borrowingTypeOf(name: String): TypeSchema = linkedProgram.borrowingTypeMap(name)
    def finalTypeOf(name: String): TypeSchema = linkedProgram.nonborrowingTypeMap(name)
    def typeOfMain = borrowingTypeOf("main")

    def callingMain: Grs = {
      val mainNode = linkedProgram.nodeMap.getOrElse("main", fail("No main function"))
      Grs(GrsApply(0).toNode(Seq(mainNode)))
    }
  }

  def compile(code: String): CompiledModule = {
    val uncompiledModule = LangParsers.parseModuleOrThrow(code, operatorSet)
    ModCompiler.withDebug(debug).compile(Stdlib.symTab, uncompiledModule)
  }

  def link(mods: CompiledModule*) = {
    ModLinker.withDebug(debug).link(mods)
  }
}
