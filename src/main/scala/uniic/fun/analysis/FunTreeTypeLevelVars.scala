package uniic.fun.analysis
import uniic.fun._

private[analysis] trait FunTreeTypeLevelVars extends FunTreeBase {
  type Node <: NodeBase with TypeLevelVarsNode

  trait TypeLevelVarsNode extends NodeBase {
    self: Node =>

    lazy val freeTypeLevelVars: Set[String] = this.expr match {
      case FunLambda(params, _) => {
        val paramFvs: Set[String] = params.map(_._2).flatMap(_.freeTLVars).toSet
        paramFvs ++ freeTypeLevelVarsBelow
      }
      case FunBuiltinFunction(_, ts, _, _) => ts.freeTLVars.map(_.name)
      case _ => freeTypeLevelVarsBelow
    }

    private def freeTypeLevelVarsBelow: Set[String] = this.children.flatMap(_.freeTypeLevelVars).toSet
  }
}