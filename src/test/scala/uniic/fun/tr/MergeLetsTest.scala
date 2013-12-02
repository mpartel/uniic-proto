package uniic.fun.tr
import uniic.fun._
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class MergeLetsTest extends FreeSpec with ShouldMatchers {
  "simple case" in {
    val input = FunLet(
      Seq("x" -> FunInt(1)),
      FunLet(
        Seq("y" -> FunInt(2)),
        FunInt(3)
      )
    )
    val expected = FunLet(
      Seq("x" -> FunInt(1), "y" -> FunInt(2)),
      FunInt(3)
    )
    MergeLets(input) should be (expected)
  }

  "nested thrice" in {
    val input = FunLet(
      Seq("x" -> FunInt(1)),
      FunLet(
        Seq("y" -> FunInt(2)),
        FunLet(
          Seq("z" -> FunInt(3)),
          FunInt(4)
        )
      )
    )
    val expected = FunLet(
      Seq("x" -> FunInt(1), "y" -> FunInt(2), "z" -> FunInt(3)),
      FunInt(4)
    )
    MergeLets(input) should be (expected)
  }

  "nested four times" in {
    val input = FunLet(
      Seq("x" -> FunInt(1)),
      FunLet(
        Seq("y" -> FunInt(2)),
        FunLet(
          Seq("z" -> FunInt(3)),
          FunLet(
            Seq("w" -> FunInt(4)),
            FunInt(5)
          )
        )
      )
    )
    val expected = FunLet(
      Seq("x" -> FunInt(1), "y" -> FunInt(2), "z" -> FunInt(3), "w" -> FunInt(4)),
      FunInt(5)
    )
    MergeLets(input) should be (expected)
  }

  "conflicting bindings" in {
    val input = FunLet(
      Seq("x" -> FunInt(1)),
      FunLet(
        Seq("y" -> FunInt(2), "x" -> FunInt(3)),
        FunInt(3)
      )
    )
    val expected = FunLet(
      Seq("x" -> FunInt(1), "y" -> FunInt(2)),
      FunLet(
        Seq("x" -> FunInt(3)),
        FunInt(3)
      )
    )
    MergeLets(input) should be (expected)
  }
}