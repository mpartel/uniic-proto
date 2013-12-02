package uniic.integration

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test.NumberedTestCases

class BasicIntegrationTest
  extends FreeSpec
  with ShouldMatchers
  with SingleFunctionItBase
  with NumberedTestCases
{
  "simple addition" in {
    """
    {
      x := 1;
      y := 2;
      return x + y;
    }
    """ ~> ("3", "Int")
  }

  "simple if-then" in {
    """
    {
      x := 2;
      if x < 3 then {
        x := x + 5;
      }
      return x;
    }
    """ ~> ("7", "Int")
  }

  "simple if-then-else" - {
    testCase {
      """
      {
        x := 5;
        if x < 3 then {
          x := x + 5;
        } else {
          x := x - 1;
        }
        return x;
      }
      """ ~> ("4", "Int")
    }

    testCase {
      """
      {
        a := 5;
        if a > 3 then {
          x := a + 5;
        } else {
          x := a - 1;
        }
        return x;
      }
      """ ~> ("10", "Int")
    }
  }

  "factorial" in {
    """
    {
      n := 5;
      i := 1;
      x := 1;
      while i <= n do {
        x := x * i;
        i := i + 1;
      }
      return x;
    }
    """ ~> ("120", "Int")
  }

  "collatz" in {
    """
    {
      count := 1;
      n := 6;
      while n > 1 do {
        if n % 2 == 0 then {
          n := n / 2;
        } else {
          n := 3 * n + 1;
        }
        count := count + 1;
      }
      return count;
    }
    """ ~> ("9", "Int")
  }

  "type error in while body" in {
    """
    {
      a := true;
      i := 0;
      while i < 5 do {
        a := 3;
      }
       return a;
    }
    """ should not (ssaTypecheck)
  }
}