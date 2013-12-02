package uniic.types

trait TypeDSL {
  class FunParamSeqWrapper(params: Seq[TFunParam]) {
    def :->(right: Type) = TFun(params, ANonUniq, right)
    def :-(a: TypeAttr) = new {
      def >(right: Type) = TFun(params, ANonUniq, right).withAttr(a)
    }
  }

  implicit def wrapType(t: Type): FunParamSeqWrapper = new FunParamSeqWrapper(Seq(TFunParam(t, MNone)))
  implicit def wrapTypeSeq(ts: Seq[Type]): FunParamSeqWrapper = new FunParamSeqWrapper(ts.map(TFunParam(_, MNone)))
  implicit def wrapFunParam(param: TFunParam): FunParamSeqWrapper = new FunParamSeqWrapper(Seq(param))
  implicit def wrapFunParamSeq(params: Seq[TFunParam]): FunParamSeqWrapper = new FunParamSeqWrapper(params)
  implicit def wrapUnit(u: Unit): FunParamSeqWrapper = wrapTypeSeq(Seq.empty)

  implicit def wrapTypeAttr(left: TypeAttr) = new {
    def unary_!() = ANot(left)
    def \/(right: TypeAttr) = AOr(left, right)
    def \/(right: String) = AOr(left, AVar(right))
    def /\(right: TypeAttr) = AAnd(left, right)
    def /\(right: String) = AAnd(left, AVar(right))
  }

  implicit def stringToAVar(s: String): AVar = AVar(s)

  def forall(vars: KindedVar*)(ty: Type) = TypeSchema(vars, ty)

  def mkTTuple(tys: Type*) = TTuple(tys)
}

object TypeDSL extends TypeDSL
