package uniic.integration
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test.NumberedTestCases

class UniquenessTypingTest
  extends FreeSpec
  with ShouldMatchers
  with SingleFunctionItBase
  with NumberedTestCases
{
  "returning unique" in {
    """
    {
      x := mkIntArray(5);
      return x;
    }
    """ |- "forall u:A. IntArray^u"
  }

  "duplicating an unique makes it shared" in {
    """
    {
      x := mkIntArray(5);
      y := (x, x);
      return y;
    }
    """ |- "forall ut:A. (IntArray, IntArray)^ut"
  }

  "consuming an unique value" - {
    testCase {
      """
      {
        a := mkIntArray(5);
        return consume(a);
      }
      """ ~> ("()", "Unit")
    }

    testCase {
      """
      {
        a := mkIntArray(5);
        consume(a);
        return a;
      }
      """ should not (ssaTypecheck)
    }
  }

  "attempting to use shared where unique required" in {
    """
    {
      x := 5;
      consume(x);
      return 0;
    }
    """ should not (ssaTypecheck)
  }

  "attempting to use unique twice" in {
    """
    {
      a := mkIntArray(5);
      consume(a);
      consume(a);
      return 0;
    }
    """ should not (ssaTypecheck)
  }

  "setting unique variable multiple times" in {
    """
    {
      a := mkIntArray(5);
      a := a.intArraySetNoBorrow(3, 10);
      (x, a) := a.intArrayGetNoBorrow(3);
      (l, a) := a.intArrayLengthNoBorrow();
      return (l, x);
    }
    """ ~> ("(5, 10)", "forall ut:A. (Int, Int)^ut")
  }

  "using unique in different if-branches" in {
    """
    {
      a := mkIntArray(5);
      if true then {
        consume(a);
      } else {
        consume(a);
      }
      return 0;
    }
    """ ~> ("0", "Int")
  }

  numberedTestCases("using unique in a loop") - {
    testCase {
      """
      {
        a := mkIntArray(5);
        i := 0;
        while i < 5 do {
          a := a.intArraySetNoBorrow(i, i * i);
          i := i + 1;
        }
        (x, a) := a.intArrayGetNoBorrow(4);
        return x;
      }
      """ ~> ("16", "Int")
    }

    testCase {
      """
      {
        a := mkIntArray(5);
        i := 0;
        while i < 5 do {
          a.intArraySetNoBorrow(i, i * i);
          i := i + 1;
        }
        (x, a) := a.intArrayGetNoBorrow(4);
        return x;
      }
      """ should not (ssaTypecheck)
    }
  }

  "nested blocks" in {
    """
    {
      a := mkIntArray(1);
      if true then {
        if true then {
          a := mkIntArray(2);
        } else {
          a := mkIntArray(3);
        }
      } else {
        a := a.intArraySetNoBorrow(0, 123);
      }
      (x, a) := a.intArrayGetNoBorrow(0);
      return a;
    }
    """ |- "forall a:A. IntArray^a"
  }

  "different but unifiable types to block" in {
    """
    {
      a := makeShared(mkIntArray(1));
      while true do {
        while true do {
          if true then {
            a := makeShared(mkIntArray(2));
          } else {
            a := mkIntArray(3);
          }
        }
      }
      return a;
    }
    """ |- "IntArray"
  }

}
