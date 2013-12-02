package uniic.fun

sealed trait Pattern {
  def patternVars: Set[String] = patternVarsSeq.toSet
  def patternVarsSeq: Seq[String]
}
case object PatUnit extends Pattern {
  def patternVarsSeq = Seq.empty
  override def toString = "()"
}
case class PatBool(value: Boolean) extends Pattern {
  def patternVarsSeq = Seq.empty
  override def toString = value.toString
}
case class PatInt(value: Int) extends Pattern {
  def patternVarsSeq = Seq.empty
  override def toString = value.toString
}
case object PatUnused extends Pattern {
  def patternVarsSeq = Seq.empty
  override def toString = "_"
}
case class PatVar(name: String) extends Pattern {
  def patternVarsSeq = Seq(name)
  override def toString = name
}
case class PatTuple(members: Seq[Pattern]) extends Pattern {
  def patternVarsSeq = members.flatMap(_.patternVarsSeq)
  override def toString = members.mkString("(", ", ", ")")
}
