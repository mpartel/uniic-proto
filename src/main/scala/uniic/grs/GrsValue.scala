package uniic.grs

import uniic.types.TypeSchema
import uniic.types.BuiltinType
import uniic.types.TEFunParam
import uniic.types.TypeBorrowingRemoval
import uniic.types.TypeAnnotation
import uniic.misc.Thunk
import uniic.misc.CollectionUtil
import uniic.misc.CompilerException
import uniic.misc.VarStream

/** The value of a graph rewrite system node. */
sealed trait GrsValue {
  type Self >: this.type <: GrsValue
  def self: Self = this

  /** Number of children */
  val arity: Int

  /** A lazy edge is one that is not evaluated by default. */
  def isLazyEdge(index: Int): Boolean = false

  /** A metadata edge is one that refcounting ignores. */
  def isMetadataEdge(index: Int): Boolean = false

  def toNode = GrsNode(self, Seq.empty)
  def toNode(children: Seq[GrsNode]) = GrsNode(self, children)
  def toNodeWithLazyChildren(children: () => Seq[GrsNode]) = GrsNode.applyLazyChildren(self, children)

  def freeTypeLevelVars: Set[String] = Set.empty
}

case object GrsRoot extends GrsValue {
  type Self = GrsRoot.type
  val arity = 1
}

/** The toplevel node in a compiled or linked module. */
case class GrsModule(entryNames: Seq[String]) extends GrsValue {
  type Self = GrsModule
  val arity = entryNames.length

  CollectionUtil.findDuplicate(entryNames).foreach { dup =>
    throw new IllegalArgumentException("GrsModule duplicate entry: " + dup)
  }
}

object GrsModule {
  def makeNode(entries: Seq[(String, GrsNode)]): GrsNodeOf[GrsModule] = {
    GrsModule(entries.map(_._1)).toNode(entries.map(_._2))
  }
}

/** FunToGrs uses this as a temporary placeholder for let-bound nodes. */
case object GrsThunk extends GrsValue {
  type Self = GrsThunk.type
  val arity = 1
}

case object GrsUnit extends GrsValue {
  type Self = GrsUnit.type
  val arity = 0
}

case class GrsBool(value: Boolean) extends GrsValue {
  type Self = GrsBool
  val arity = 0
}

case class GrsInt(value: Int) extends GrsValue {
  type Self = GrsInt
  val arity = 0
}

case class GrsTuple(arity: Int) extends GrsValue {
  type Self = GrsTuple
}

case class GrsParamList(arity: Int) extends GrsValue {
  type Self = GrsParamList
}

/** For a lambda node, points to all nodes that are reachable from
  * the lambda node as well as some ancestor of the lambda node. */
case class GrsClosure(arity: Int) extends GrsValue {
  type Self = GrsClosure
}

/** The first child is a GrsParamList of parameters.
  * The second child is a GrsClosure.
  * The third child is the body.
  * The first and third child are metadata edges. */
case class GrsLambda(paramTypes: Seq[TypeAnnotation]) extends GrsValue {
  type Self = GrsLambda
  val arity = 3

  override def isLazyEdge(index: Int) = index == 0 || index == 2
  override def isMetadataEdge(index: Int) = index == 0 || index == 2

  override def freeTypeLevelVars = paramTypes.flatMap(_.freeTLVars).toSet
}

case class GrsBuiltinFunction(
  name: String,
  paramNames: Seq[String],
  impl: Seq[GrsNode] => GrsNode
) extends GrsValue
{
  type Self = GrsBuiltinFunction
  val arity = 0

  override def toString = s"GrsBuiltinFunction($name)"
}

case class GrsBuiltinValue(value: Any, ty: Option[BuiltinType]) extends GrsValue {
  type Self = GrsBuiltinValue
  val arity = 0
  override def freeTypeLevelVars = ty.map(_.freeTLVars.map(_.name)).getOrElse(Set.empty)
}

case class GrsVar(name: String) extends GrsValue {
  type Self = GrsVar
  val arity = 0
}

/** Variables that refer to external objects get changed into these during compilation.
  *
  * These are removed by the linker. */
case class GrsExternal(name: String) extends GrsValue {
  type Self = GrsExternal
  val arity = 0
}

case class GrsApply(val argCount: Int) extends GrsValue {
  type Self = GrsApply
  val arity = 1 + argCount
}

case object GrsIfThenElse extends GrsValue {
  type Self = GrsIfThenElse.type
  val arity = 3

  override def isLazyEdge(index: Int) = index > 0
}

/** The first child is the head, the rest are GrsCases. */
case class GrsMatch(numCases: Int) extends GrsValue {
  type Self = GrsMatch
  val arity = 1 + numCases
}

/** The first child is the pattern and the second child is the body. */
case object GrsCase extends GrsValue {
  type Self = GrsCase.type
  val arity = 2
  override def isLazyEdge(index: Int) = true
  override def isMetadataEdge(index: Int) = index == 0
}

/** The unused value placeholder in patterns. */
case object GrsUnused extends GrsValue {
  type Self = GrsUnused.type
  val arity = 0
}
