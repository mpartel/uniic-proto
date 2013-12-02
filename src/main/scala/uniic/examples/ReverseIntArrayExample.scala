package uniic.examples

import uniic.grs.GrsDSL
import uniic.grs.GrsBuiltinValue
import uniic.stdlib.Stdlib

object ReverseIntArrayExample extends Example with GrsDSL {
  def supportCode =
    """
    module ReverseIntArrayExample

    def intSwap(a: @IntArray*, i: Int, j: Int): Unit {
      x := @a.intArrayGet(i);
      y := @a.intArrayGet(j);
      @a.intArraySet(i, y);
      @a.intArraySet(j, x);
      return ();
    }

    def test1(): IntArray* {
      a := mkIntArray(5);
      i := 0;
      while i < 5 do {
        @a.intArraySet(i, 101 + i);
        i := i + 1;
      }
      reverseIntArray(@a);
      return a;
    }
    """

  def visibleCode =
    """
    def reverseIntArray(a: @IntArray*): Unit {
      len := intArrayLength(@a);
      i := 0;
      while i <= len / 2 do {
        intSwap(@a, len - i - 1, i);
        i := i + 1;
      }
      return ();
    }
    """

  def fullCode = supportCode + "\n" + visibleCode

  def testCase1() = {
    val result = runParameterlessFunction("test1")
    def failResult() = fail("Unexpected result: " + result.rootNode.soleChild)

    Stdlib.intArrayConversion.fromNode.lift(result.rootNode.soleChild) match {
      case Some(a) => {
        if (a.toList != List(105, 104, 103, 102, 101)) {
          fail("Unexpected array contents: " + a.toSeq.mkString("(", ",", ")"))
        }
      }
      case _ => fail("Unexpected result: " + result.rootNode.soleChild)
    }
  }

  def main(args: Array[String]) {
    testCase1()
    println("Test case 1 OK")
  }
}
