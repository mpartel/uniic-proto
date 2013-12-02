package uniic.fun.tr
import uniic.fun._
import org.scalatest.FreeSpec
import uniic.test.ImplicitlyParsedFunExprs

class ConstantSharingTest
  extends FreeSpec
  with ImplicitlyParsedFunExprs
{
  "constant sharing" in {
    val tree = parseFunTree(
      """
      (0, 1, 0, true, false, false, (), ())
      """
    )

    val result = ConstantSharing(tree)
    val expected = parseFunTree(
      """
      let $BoolF = false
      and $Int0 = 0
      and $Unit = ()
      in ($Int0, 1, $Int0, true, $BoolF, $BoolF, $Unit, $Unit)
      """
    )

    assert(result.rootExpr === expected.rootExpr)
  }

  "when nothing to do" in {
    val tree = parseFunTree(
      """
      (0, 1, true, false, ())
      """
    )

    assert(ConstantSharing(tree).rootExpr == tree.rootExpr)
  }
}
