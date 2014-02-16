package uniic.examples

import uniic.module._
import uniic.grs._
import uniic.stdlib.Stdlib
import uniic.parsers.LangParsers

abstract class Example {
  def visibleCode: String
  def fullCode: String

  lazy val modCompiler = ModCompiler
  lazy val modLinker = ModLinker

  lazy val parsedModule = LangParsers.parseModuleOrThrow(fullCode, Stdlib.defaultOperatorSet)
  lazy val compiledModule = modCompiler.compile(Stdlib.symTab, parsedModule)
  lazy val linkedProgram = modLinker.link(Seq(Stdlib.asModule, compiledModule))

  def runParameterlessFunction(name: String): Grs = {
    val lambdaNode = linkedProgram.nodeMap(name)
    assert(lambdaNode.value.isInstanceOf[GrsLambda])
    val grsToEval = Grs(GrsApply(0).toNode(Seq(lambdaNode)))

    GrsEval.eval(grsToEval)
  }

  def fail(msg: String) = throw new AssertionError(msg)
}
