package uniic.grs

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test.GrsTestUtils
import uniic.test.NumberedTestCases
import uniic.parsers.LangParsers
import uniic.stdlib.Stdlib
import uniic.fun.FunToGrs
import uniic.types.MissingTypeAnnotation

class GrsRefCountsTest
  extends FreeSpec
  with GrsTestUtils
  with NumberedTestCases
{
  case class Result[K](refCounts: GrsRefCounts, nodeMap: Map[K, GrsNode])

  def countRefs[K](specs: GrsSpec[K]*): Result[K] = {
    val (grs, nodeMap) = mkGrsAndNodeMap(specs: _*)
    Result(grs.refCounts, nodeMap)
  }

  def refCountOf[K](nodeKey: K)(implicit result: Result[K]): RefCount = {
    val node = result.nodeMap(nodeKey)
    result.refCounts.forNode(node)
  }

  def assertShared[K](nodeKey: K)(implicit result: Result[K]) {
    val rc = refCountOf(nodeKey)
    if (rc != SharedRef) {
      fail(nodeKey + " was not shared")
    }
  }

  def assertUnshared[K](nodeKey: K)(implicit result: Result[K]) {
    val rc = refCountOf(nodeKey)
    if (rc != UnsharedRef) {
      fail(nodeKey + " was shared")
    }
  }

  "unshared pair" in {
    implicit val result = countRefs(
      'tuple := GrsTuple(2) | ('one, 'two),
      'one := GrsInt(1),
      'two := GrsInt(2)
    )
    assertUnshared('one)
    assertUnshared('two)
  }

  "shared pair" in {
    implicit val result = countRefs(
      'tuple := GrsTuple(2) | ('one, 'one),
      'one := GrsInt(1)
    )
    assertShared('one)
  }

  "diamond shape" in {
    implicit val result = countRefs(
      'top := GrsTuple(2) | ('left, 'right),
      'left := GrsTuple(2) | ('one, 'bottom),
      'right := GrsTuple(2) | ('bottom, 'two),
      'bottom := GrsInt(123),
      'one := GrsInt(1),
      'two := GrsInt(2)
    )
    assertShared('bottom)
    assertUnshared('left)
    assertUnshared('right)
  }

  numberedTestCases("alternative execution paths in if-then-else") - {
    testCase {
      implicit val result = countRefs(
        'if := GrsIfThenElse | ('x, 'y, 'z),
        'x := GrsVar("x"),
        'y := GrsVar("y"),
        'z := GrsVar("z")
      )
      assertUnshared('x)
      assertUnshared('y)
      assertUnshared('z)
    }
    testCase {
      implicit val result = countRefs(
        'if := GrsIfThenElse | ('x, 'y, 'y),
        'x := GrsVar("x"),
        'y := GrsVar("y")
      )
      assertUnshared('x)
      assertUnshared('y)
    }
    testCase {
      implicit val result = countRefs(
        'if := GrsIfThenElse | ('x, 'x, 'y),
        'x := GrsVar("x"),
        'y := GrsVar("y")
      )
      assertShared('x) // A more accurate implementation could the second 'x as unshared.
      assertUnshared('y)
    }
  }

  "does not consider patterns in case statements" in {
    implicit val result = countRefs(
      'match := GrsMatch(1) | ('head, 'case),
      'head := GrsInt(1),
      'case := GrsCase | ('x, 'x),
      'x := GrsVar("x")
    )
    assertUnshared('x)
  }

  "cyclic graphs" in {
    implicit val result = countRefs(
      'lambda := GrsLambda(Seq(MissingTypeAnnotation)) | ('params, 'fvs, 'body),
      'params := GrsParamList(1) | ('x),
      'fvs := GrsClosure(0),
      'body := GrsTuple(3) | ('x, 'lambda, 'lambda),
      'x := GrsVar("x")
    )
    assertUnshared('x)
    assertShared('lambda)
  }

  "lambda variable declarations are metadata edges" in {
    implicit val result = countRefs(
      'lambda := GrsLambda(Seq(MissingTypeAnnotation)) | ('params, 'fvs, 'body),
      'params := GrsParamList(1) | ('x),
      'fvs := GrsClosure(0),
      'body := GrsTuple(2) | ('x, 'y),
      'x := GrsVar("x"),
      'y := GrsVar("y")
    )
    assertUnshared('fvs)
    assertUnshared('x)
    assertUnshared('y)
  }

  "paths via lambdas don't count" in {
    implicit val result = countRefs(
      'f := GrsLambda(Seq()) | ('params, 'fvs, 'body),
      'params := GrsParamList(0),
      'fvs := GrsClosure(0),
      'body := GrsIfThenElse | ('p, 'fa, 'ga),
      'fa := GrsApply(1) | ('f, 'a),
      'ga := GrsApply(1) | ('g, 'a),
      'g := GrsVar("g"),
      'p := GrsVar("p"),
      'a := GrsVar("a")
    )
    assertUnshared('a)
  }

  "two paths to if node whose then and else coincide" in {
    implicit val result = countRefs(
      'lambda := GrsLambda(Seq(MissingTypeAnnotation, MissingTypeAnnotation)) | ('params, 'fvs, 'body),
      'params := GrsParamList(2) | ('x, 'y),
      'fvs := GrsClosure(0),
      'body := GrsTuple(2) | ('if, 'right),
      'right := GrsTuple(2) | ('if, 'z),
      'if := GrsIfThenElse | ('x, 'y, 'y),
      'x := GrsVar("x"),
      'y := GrsVar("y"),
      'z := GrsVar("z")
    )
    assertUnshared('body)
    assertShared('if)
    assertUnshared('right)
    assertUnshared('x)
    assertUnshared('y)
    assertUnshared('z)
  }
}
