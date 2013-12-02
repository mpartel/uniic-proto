package uniic.parsers
import uniic.fun._
import uniic.types._
import uniic.stdlib.Stdlib
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.parsing.input.Reader

class FunParsersTest
    extends FreeSpec
    with ShouldMatchers
    with ParsersTestBase
    with FunDSL {
  object ParsersImpl extends FunParsers {
    override val infixOperators = Stdlib.defaultOperatorSet
  }

  override val parsers = ParsersImpl // ParsersImpl exists to work around a Scala bug ("not found: type $anon")
  override def mkScanner(reader: Reader[Char]) = new parsers.lexical.Scanner(reader)
  import parsers._

  def p(s: String): ParseResult[FunExpr] = s parsedWith funExpr
  def ap(fun: FunExpr, args: FunExpr*) = FunApply(fun, args)

  val MTA = MissingTypeAnnotation

  implicit def stringToVar(s: String) = FunVar(s)
  implicit def stringSeqToVarSeq(ss: Seq[String]) = ss.map(FunVar(_))

  "constants" in {
    p("()") should parseAs (FunUnit)
    p("true") should parseAs (FunBool(true))
    p("false") should parseAs (FunBool(false))
    p("0") should parseAs (FunInt(0))
    p("123") should parseAs (FunInt(123))
    p("-123") should parseAs (FunInt(-123))
  }

  "superfluous parens" in {
    p("((((123))))") should parseAs (FunInt(123))
  }

  "variables" in {
    p("x") should parseAs (FunVar("x"))
    p("xoox1") should parseAs (FunVar("xoox1"))
  }

  "function application" in {
    p("f(x)") should parseAs (ap("f", "x"))
    p("f(x, y)") should parseAs (ap("f", "x", "y"))
    p("f(x, y, z)") should parseAs (ap("f", "x", "y", "z"))
    p("f(g(x))") should parseAs (ap("f", ap("g", "x")))
  }

  "lambda expression" in {
    p("\\ x . f(x)") should parseAs (FunLambda("x", ap("f", "x")))
    p("\\x . f(x)") should parseAs (FunLambda("x", ap("f", "x")))
    p("\\x. f(x)") should parseAs (FunLambda("x", ap("f", "x")))
    p("\\x.f(x)") should parseAs (FunLambda("x", ap("f", "x")))
    p("(((\\x.f(x))))") should parseAs (FunLambda("x", ap("f", "x")))
    p("λx.f(x)") should parseAs (FunLambda("x", ap("f", "x")))
  }

  "lambda expression with multiple parameters" in {
    p("\\ x, y .f(x)") should parseAs (FunLambda(Seq("x" -> MTA, "y" -> MTA), ap("f", "x")))
    p("\\x, y.f(x)") should parseAs (FunLambda(Seq("x" -> MTA, "y" -> MTA), ap("f", "x")))
    p("λx, y.f(x)") should parseAs (FunLambda(Seq("x" -> MTA, "y" -> MTA), ap("f", "x")))
  }

  "lambda expression and immediate application" in {
    p("(\\x.x)(3)") should parseAs (ap(FunLambda(Seq("x" -> MTA), "x"), FunInt(3)))
  }

  " lambda expression with zero parameters" in {
    p("\\.5") should parseAs (FunLambda(Seq.empty, FunInt(5)))
  }

  "lambda expression with parameter types" in {
    val expectedParams = Seq(
      "x" -> TypeExprTypeAnnotation(TEVar("Int").withAttr(TEUniq)),
      "y" -> TypeExprTypeAnnotation(TEVar("t").withAttr(TEVar("u"))),
      "z" -> TypeExprTypeAnnotation(TEVar("t"))
    )
    p("\\x : Int*, y : t^u, z : t . x") should parseAs (FunLambda(expectedParams, FunVar("x")))
    p("\\x:Int*, y:t^u, z : t . x") should parseAs (FunLambda(expectedParams, FunVar("x")))
  }

  "let expressions" in {
    p("let x = 3 in x") should parseAs (FunLet(Seq("x" -> 3), "x"))
    p("let f = \\x, y. y in f") should parseAs (FunLet(Seq("f" -> FunLambda(Seq("x" -> MTA, "y" -> MTA), "y")), "f"))
    p("let f = \\x, y. let z = y in z in f") should parseAs (
      FunLet(Seq("f" -> FunLambda(
        Seq("x" -> MTA, "y" -> MTA),
        FunLet(Seq("z" -> "y"), "z")
        )),
      "f")
    )
  }

  "multiple let declarations separated by 'and'" in {
    p("let x = 3 and y = 4 in true") should parseAs(FunLet(Seq("x" -> 3, "y" -> 4), FunBool(true)))
  }

  "if expressions" in {
    p("if true then 1 else 2") should parseAs (FunIfThenElse(true, 1, 2))
    p("if true then 1") should not be ('successful)
    p("if f(x) then g(y) else g(z)") should parseAs (FunIfThenElse(ap("f", "x"), ap("g", "y"), ap("g", "z")))
    p("if 1 + 1 == 2 then g(y) else g(z)") should parseAs (FunIfThenElse(ap("==", ap("+", 1, 1), 2), ap("g", "y"), ap("g", "z")))
  }

  "nested if expressions" in {
    p("if true then if false then 1 else 2 else 3") should parseAs(FunIfThenElse(true, FunIfThenElse(false, 1, 2), 3))
    p("if true then 1 else if false then 2 else 3") should parseAs(FunIfThenElse(true, 1, FunIfThenElse(false, 2, 3)))
    p("if true then a(b) else if c(d) then 2 else 3") should parseAs(FunIfThenElse(true, ap("a", "b"), FunIfThenElse(ap("c", "d"), 2, 3)))
    p("if true then a(b) else if false then c(d) else 2") should parseAs(FunIfThenElse(true, ap("a", "b"), FunIfThenElse(false, ap("c", "d"), 2)))
  }

  "tuple construction" in {
    p("(1, 2, 3)") should parseAs (FunTupleExpr(Seq(1, 2, 3)))
    p("(1,2,3)") should parseAs (FunTupleExpr(Seq(1, 2, 3)))
  }

  "simple binary operators" in {
    p("1 + 2") should parseAs (ap("+", 1, 2))
  }

  "left associative binary operators" in {
    p("1 + 2 + 3") should parseAs (ap("+", ap("+", 1, 2), 3))
  }

  "right associative binary operators" in {
    p("1 ** 2 ** 3") should parseAs (ap("**", 1, ap("**", 2, 3)))
  }

  "unassociative binary operators" in {
    p("1 == 2") should parseAs (ap("==", 1, 2))
    p("1 == 2 == 3") should not be ('successful)
  }

  "binary operator precedence" in {
    p("1 + 2 * 3") should parseAs (ap("+", 1, ap("*", 2, 3)))
    p("1 * 2 + 3") should parseAs (ap("+", ap("*", 1, 2), 3))
    p("1 + 2 == 3") should parseAs (ap("==", ap("+", 1, 2), 3))
  }

  "binary operators inside an if clause" in {
    p("if x <= 1 then 1 else 2") should parseAs (FunIfThenElse(ap("<=", "x", 1), 1, 2))
  }

  "binary operators inside a lambda" in {
    p("\\x. x <= 1") should parseAs (FunLambda("x", ap("<=", "x", 1)))
  }

  "binary operators inside a let" in {
    p("let p = x <= 1 in p") should parseAs (FunLet(Seq("p" -> ap("<=", "x", 1)), "p"))
  }

  "binary operators with mixed associativity and precedence" in {
    p("1 + 2 * 3 <= 5 + 5 * 5") should parseAs (
      ap("<=",
        ap("+",
          1,
          ap("*", 2, 3)
        ),
        ap("+",
          5,
          ap("*", 5, 5)
        )
      )
    )
  }

  "a more complex expression" in {
    p("""
      let fact = \n.
        if n <= 1
          then 1
          else n * fact(n - 1)
      in fact(5)
      """) should be ('successful)
  }

  "pattern matching" in {
    p("1 match { case 1 => true; case 2 => false }") should parseAs (
      FunMatch(1, Seq(
        FunCase(PatInt(1), true),
        FunCase(PatInt(2), false)
      ))
    )
    p("(1, 2) match { case (x, (z, y)) => true; case _ => false }") should parseAs (
      FunMatch(FunTupleExpr(Seq(1, 2)), Seq(
        FunCase((PatTuple(Seq(PatVar("x"), PatTuple(Seq(PatVar("z"), PatVar("y")))))), true),
        FunCase(PatUnused, false)
      ))
    )
  }
}
