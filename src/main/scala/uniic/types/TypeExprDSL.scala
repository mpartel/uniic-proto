package uniic.types

trait TypeExprDSL {
  val tbool = TEVar("Bool")
  val tint = TEVar("Int")

  implicit def stringToVar(s: String) = TEVar(s)
  implicit def wrapTypeExpr(te: TypeExpr) = new TypeExprWrapper(te)
  implicit def wrapTypeExpr(s: String) = new TypeExprWrapper(TEVar(s))
  implicit def wrapFunParam(p: TEFunParam) = new FunParamWrapper(p)

  class TypeExprWrapper(left: TypeExpr) {
    def :->(right: TypeExpr) = TEFun(Seq(TEFunParam(left, MNone)), TENonUniq, right)
    def :-(a: TypeExpr) = new {
      def >(right: TypeExpr) = TEFun(Seq(TEFunParam(left, MNone)), TENonUniq, right).withAttr(a)
    }

    def unary_! = TENot(left)
    def \/(right: TypeExpr) = TEOr(left, right)
    def \/(right: String) = TEOr(left, TEVar(right))
    def /\(right: TypeExpr) = TEAnd(left, right)
    def /\(right: String) = TEAnd(left, TEVar(right))
  }

  class FunParamWrapper(left: TEFunParam) {
    def :->(right: TypeExpr) = TEFun(Seq(left), TENonUniq, right)
    def :-(a: TypeExpr) = new {
      def >(right: TypeExpr) = TEFun(Seq(left), TENonUniq, right).withAttr(a)
    }
  }

  def forall(vars: (String, Kind)*)(ty: TypeExpr) = TESchema(vars, ty)

  def mkTETuple(tys: TypeExpr*) = TETuple(tys)
}
