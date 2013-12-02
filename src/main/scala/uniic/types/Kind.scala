package uniic.types

abstract class Kind {
  def name: String
}
case object KBaseType extends Kind {
  def name = "base type"
}
case object KTypeAttr extends Kind {
  def name = "type attribute"
}
case object KType extends Kind {
  def name = "type"
}
case object KTypeSchema extends Kind {
  def name = "type schema"
}
