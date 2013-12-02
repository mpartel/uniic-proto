package uniic.boolalg
import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryBoolTerms {
  implicit def arbitraryBoolTerm: Arbitrary[BoolTerm] = {
    Arbitrary {
      val varNames = Gen.oneOf("a", "b", "c", "x", "y", "z")

      val genConst = Gen.oneOf(BTrue, BFalse)
      val genVar = for (v <- varNames) yield BVar(v)

      def genBinary(f: (BoolTerm, BoolTerm) => BoolTerm)(sz: Int): Gen[BoolTerm] = for {
        l <- sizedTerm(sz/2)
        r <- sizedTerm(sz/2)
      } yield f(l, r)

      def genAnd = genBinary(BAnd(_, _))(_)
      def genOr = genBinary(BOr(_, _))(_)
      def genNot(sz: Int) = for (x <- sizedTerm(sz - 1)) yield BNot(x)

      def sizedTerm(sz: Int): Gen[BoolTerm] = {
        if (sz <= 1) genVar
        else Gen.frequency(
          (1, Gen.lzy(genConst)),
          (2, Gen.lzy(genVar)),
          (4, Gen.lzy(genAnd(sz))),
          (4, Gen.lzy(genOr(sz))),
          (1, Gen.lzy(genNot(sz)))
        )
      }

      // The standard input size is between 0 and 100.
      // We'll scale it to 0..40 i.e. at most about 5 levels deep
      //val scale = 40
      val scale = 100
      Gen.sized(sz => sizedTerm(sz * scale / 100))
    }
  }
}

object ArbitraryBoolTerms extends ArbitraryBoolTerms
