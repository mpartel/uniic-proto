package uniic
import uniic.types._
import uniic.parsers._
import uniic.misc._
import uniic.fun._

object Main extends App {
  println("TODO")
  /*
  val input = scala.io.Source.stdin.mkString
  println("Input:\n" + input)

  val expr = LangParsers.parseFunExpr(input, Stdlib.defaultOperatorSet) match {
    case Right(fe) => fe
    case Left(error) => throw new CompilerException(error)
  }
  val (rootNode, typeMap) = FunToGraph.apply(new FunToGraph.Params {
    val funExpr = expr
    val symTab = Stdlib.symTab
    val typeSymTab = Stdlib.typeSymTab
  })
  val ty = TgrsTyping.getType(new TgrsTyping.Params {
    val root = rootNode
    val typeAssignments = typeMap
    val rigidTypeVars = Set.empty[String]
  })
  println("Type: " + ty)
  println("Running...")
  TgrsEval.apply(rootNode)(TgrsEval.NoTimeLimit)
  println(rootNode.value)
  */
}
