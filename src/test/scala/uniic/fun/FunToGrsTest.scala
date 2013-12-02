package uniic.fun

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test._
import uniic.grs._
import uniic.types.MissingTypeAnnotation

class FunToGrsTest
  extends FreeSpec
  with ShouldMatchers
  with ImplicitlyParsedFunExprs
  with GrsTestUtils
  with NumberedTestCases
{
  val MTA = MissingTypeAnnotation

  "primitives" in {
    FunToGrs("()", Map.empty) should beEquivalentTo (
      'root := GrsUnit
    )

    FunToGrs("true", Map.empty) should beEquivalentTo (
      'root := GrsBool(true)
    )

    FunToGrs("3", Map.empty) should beEquivalentTo (
      'root := GrsInt(3)
    )
  }

  numberedTestCases("lambdas") - {
    testCase {
      FunToGrs("""\x. x""", Map.empty) should beEquivalentTo (
        'root := GrsLambda(Seq(MTA)) | ('params, 'closure, 'x),
        'params := GrsParamList(1) | ('x),
        'closure := GrsClosure(0),
        'x := GrsVar("x")
      )
    }

    testCase {
      FunToGrs("""\x. \y. (x, y)""", Map.empty) should beEquivalentTo (
        'root := GrsLambda(Seq(MTA)) | ('params1, 'closure1, 'body1),
        'params1 := GrsParamList(1) | ('x),
        'closure1 := GrsClosure(0),
        'body1 := GrsLambda(Seq(MTA)) | ('params2, 'closure2, 'body2),
        'params2 := GrsParamList(1) | ('y),
        'closure2 := GrsClosure(1) | ('x),
        'body2 := GrsTuple(2) | ('x, 'y),
        'x := GrsVar("x"),
        'y := GrsVar("y")
      )
    }
  }

  numberedTestCases("lambda applications") - {
    testCase {
      FunToGrs("(\\x, y. x)(1, 2)", Map.empty) should beEquivalentTo (
        'apply := GrsApply(2) | ('lambda, 'one, 'two),
        'lambda := GrsLambda(Seq(MTA, MTA)) | ('params, 'closure, 'x),
        'params := GrsParamList(2) | ('x, 'y),
        'closure := GrsClosure(0),
        'x := GrsVar("x"),
        'y := GrsVar("y"),
        'one := GrsInt(1),
        'two := GrsInt(2)
      )
    }

    testCase {
      FunToGrs("(\\x. \\y. (x, x))(1)(2)", Map.empty)  should beEquivalentTo (
        'apply2 := GrsApply(1) | ('apply1, 'two),
        'apply1 := GrsApply(1) | ('outerLambda, 'one),
        'outerLambda := GrsLambda(Seq(MTA)) | ('outerParams, 'outerClosure, 'innerLambda),
        'outerParams := GrsParamList(1) | ('x),
        'outerClosure := GrsClosure(0),
        'innerLambda := GrsLambda(Seq(MTA)) | ('innerParams, 'innerClosure, 'innerBody),
        'innerParams := GrsParamList(1) | ('y),
        'innerClosure := GrsClosure(1) | ('x),
        'innerBody := GrsTuple(2) | ('x, 'x),
        'x := GrsVar("x"),
        'y := GrsVar("y"),
        'one := GrsInt(1),
        'two := GrsInt(2)
      )
    }
  }

  numberedTestCases("let-bindings") - {
    testCase {
      FunToGrs("""let x = 3 in x""", Map.empty) should beEquivalentTo (
        'root := GrsInt(3)
      )
    }
    testCase {
      FunToGrs("""let x = y and y = 3 in x""", Map.empty) should beEquivalentTo (
        'root := GrsInt(3)
      )
    }
  }

  "let-binding a recursive definition" in {
    FunToGrs("""let x = (3, x) in x""", Map.empty) should beEquivalentTo (
      'tuple := GrsTuple(2) | ('three, 'tuple),
      'three := GrsInt(3)
    )
  }

  numberedTestCases("nested let-bindings") - {
    testCase {
      FunToGrs("""let x = 3 in let y = x in x""", Map.empty) should beEquivalentTo (
        'root := GrsInt(3)
      )
    }
    testCase {
      FunToGrs("""let x = 3 in let y = x in (x, y)""", Map.empty) should beEquivalentTo (
        'root := GrsTuple(2) | ('three, 'three),
        'three := GrsInt(3)
      )
    }
  }

  numberedTestCases("cyclic let-bindings") - {
    testCase {
      FunToGrs("""let x = (1, y) and y = (2, x) in y""", Map.empty) should beEquivalentTo (
        'y := GrsTuple(2) | ('two, 'x),
        'x := GrsTuple(2) | ('one, 'y),
        'one := GrsInt(1),
        'two := GrsInt(2)
      )
    }
    testCase {
      FunToGrs("""let x = (1, y) and y = (2, z) and z = (x, y) in x""", Map.empty) should beEquivalentTo (
        'x := GrsTuple(2) | ('one, 'y),
        'y := GrsTuple(2) | ('two, 'z),
        'z := GrsTuple(2) | ('x, 'y),
        'one := GrsInt(1),
        'two := GrsInt(2)
      )
    }
  }

  numberedTestCases("cyclic aliases in let-bindings") - {
    testCase {
      evaluating {
        FunToGrs("""let x = y and y = x in x""", Map.empty)
      } should produce [FunToGrs.CyclicLetException]
    }
    testCase {
      evaluating {
        FunToGrs("""let x = y and y = z and z = x in x""", Map.empty)
      } should produce [FunToGrs.CyclicLetException]
    }
  }

  "let in let" in {
    FunToGrs("let x = let y = (y, x) in y in x", Map.empty) should beEquivalentTo (
      'x := GrsTuple(2) | ('x, 'x)
    )
  }

  "self-referential lambda" - {
    "as non-root node" in {
      FunToGrs("""let f = \x. (f, x) in (f, 0)""", Map.empty) should beEquivalentTo (
        'root := GrsTuple(2) | ('lambda, 'zero),
        'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'tuple),
        'params := GrsParamList(1) | ('x),
        'closure := GrsClosure(1) | ('lambda),
        'tuple := GrsTuple(2) | ('lambda, 'x),
        'x := GrsVar("x"),
        'zero := GrsInt(0)
      )
    }

    "as root node" in {
      FunToGrs("""let f = \x. (f, x) in f""", Map.empty) should beEquivalentTo (
        'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'tuple),
        'params := GrsParamList(1) | ('x),
        'closure := GrsClosure(1) | ('lambda),
        'tuple := GrsTuple(2) | ('lambda, 'x),
        'x := GrsVar("x")
      )
    }

    "with direct self-reference" - {
      "as non-root node" in {
        FunToGrs("""let f = \x. f in (f, 0)""", Map.empty) should beEquivalentTo (
          'root := GrsTuple(2) | ('lambda, 'zero),
          'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'lambda),
          'params := GrsParamList(1) | ('x),
          'closure := GrsClosure(1) | ('lambda),
          'x := GrsVar("x"),
          'zero := GrsInt(0)
        )
      }

      "as root node" in {
        FunToGrs("""let f = \x. f in f""", Map.empty) should beEquivalentTo (
          'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'lambda),
          'params := GrsParamList(1) | ('x),
          'closure := GrsClosure(1) | ('lambda),
          'x := GrsVar("x")
        )
      }
    }

    "with indirect self-reference" in {
      FunToGrs("""let f = \x. p and p = (3, f) in p""", Map.empty) should beEquivalentTo (
        'root := GrsTuple(2) | ('three, 'lambda),
        'three := GrsInt(3),
        'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'root),
        'params := GrsParamList(1) | ('x),
        'closure := GrsClosure(1) | ('root),
        'x := GrsVar("x")
      )
    }
  }

  "case expressions" in {
    FunToGrs("""x match { case (1, x) => x; case (_, _) => 5 }""", Map.empty) should beEquivalentTo (
      'match := GrsMatch(2) | ('head, 'case1, 'case2),
      'head := GrsVar("x"),
      'case1 := GrsCase | ('pattern1, 'x),
      'pattern1 := GrsTuple(2) | ('one, 'x),
      'one := GrsInt(1),
      'x := GrsVar("x"),
      'case2 := GrsCase | ('pattern2, 'five),
      'pattern2 := GrsTuple(2) | ('unused1, 'unused2),
      'unused1 := GrsUnused,
      'unused2 := GrsUnused,
      'five := GrsInt(5)
    )
  }

  "constant sharing" in {
    FunToGrs("""(1, 1)""", Map.empty) should beEquivalentTo (
      'tuple := GrsTuple(2) | ('one, 'one),
      'one := GrsInt(1)
    )
  }

  "factorial" in {
    val code =
      """
      let fact = \n.
        if n <= 1
          then 1
          else n * fact(n - 1)
      in fact
      """
    FunToGrs(code, Map.empty) should beEquivalentTo (
      'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'if),
      'params := GrsParamList(1) | ('n),
      'closure := GrsClosure(1) | ('lambda),
      'if := GrsIfThenElse | ('cond, 'one, 'else),
      'cond := GrsApply(2) | ('le, 'n, 'one),
      'else := GrsApply(2) | ('times, 'n, 'recursiveCall),
      'recursiveCall := GrsApply(1) | ('lambda, 'minusOne),
      'minusOne := GrsApply(2) | ('minus, 'n, 'one),
      'le := GrsVar("<="),
      'times := GrsVar("*"),
      'minus := GrsVar("-"),
      'one := GrsInt(1),
      'n := GrsVar("n")
    )
  }
}
