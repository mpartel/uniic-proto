package uniic.misc

trait UnicodeSymbols {
  val lambdaSymbol = 'λ'
  val forallSymbol = '∀'
  val gammaSymbol = 'Γ'
  val tauSymbol = 'τ'
  val upsilonSymbol = 'υ'
  val muSymbol = 'μ'
  val phiSymbol = 'φ'
  val dotSymbol = '•'
  val crossSymbol = '×'
  val funcCompositionSymbol = '∘'
  val leftArrowSymbol = '←'
  val rightArrowSymbol = '→'
  val provesSymbol = '⊢'
  val negationSymbol = '¬'
  val disjunctionSymbol = '∨'
  val conjunctionSymbol = '∧'
  val oDotSymbol = '⊙'
  val oTimesSymbol = '⊗'
  val bigODotSymbol = '⨀'
  val bigOTimesSymbol = '⨂'
}

object Unicode extends UnicodeSymbols {
  val subscriptDigits = "₀₁₂₃₄₅₆₇₈₉".toSeq
  val subscriptMinus = '₋'

  def subscript(n: Int): String = {
    val subscriptZero = subscriptDigits(0)
    if (n >= 0)
      if (n < 10) "" + (subscriptZero.toInt + n).toChar
      else subscript(n / 10) + subscript(n % 10)
    else subscriptMinus + subscript(-n)
  }
}