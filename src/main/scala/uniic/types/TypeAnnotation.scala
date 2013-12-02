package uniic.types

sealed trait TypeAnnotation {
  def freeTLVars: Set[String]
}
case object MissingTypeAnnotation extends TypeAnnotation {
  def freeTLVars = Set.empty
}
case class TypeExprTypeAnnotation(te: TypeExpr) extends TypeAnnotation {
  def freeTLVars = te.freeTLVars
}
case class SimpleTypeAnnotation(ty: Type) extends TypeAnnotation {
  def freeTLVars = ty.freeTLVars.map(_.name)
}

object TypeAnnotation {
  def apply(ty: Type) = SimpleTypeAnnotation(ty)
  def apply(te: TypeExpr) = TypeExprTypeAnnotation(te)
  val empty = MissingTypeAnnotation
}
