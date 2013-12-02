package uniic.parsers
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.stdlib.Stdlib
import uniic.imp._
import uniic.fun._
import uniic.types._
import scala.util.parsing.input.Reader

class ImpParsersTest extends FreeSpec with ShouldMatchers with ParsersTestBase {
  object ParsersImpl extends ImpParsers {
    override val infixOperators = Stdlib.defaultOperatorSet
  }

  val parsers = ParsersImpl
  def mkScanner(reader: Reader[Char]) = new parsers.lexical.Scanner(reader)
  import parsers._

  implicit def defaultToNormalArg(e: ImpExpr) = ImpNormalArg(e)

  def p(s: String): ParseResult[ImpStmt] = s parsedWith impStmt

  "returns" in {
    p("return 1 + 1;") should parseAs (
      ImpReturn(ImpCall(ImpVar("+"), Seq(ImpInt(1), ImpInt(1))))
    )
  }

  "operator precedence" in {
    p("x := y * k + 2;") should parseAs (
      ImpSet(
        ImpVar("x"),
        ImpCall(
          ImpVar("+"),
          Seq(
            ImpCall(
              ImpVar("*"),
              Seq(ImpVar("y"), ImpVar("k"))
            ),
            ImpInt(2)
          )
        )
      )
    )
  }

  "making tuples" in {
    p("x := (a, b);") should parseAs (
      ImpSet(ImpVar("x"), ImpMakeTuple(Seq(ImpVar("a"), ImpVar("b"))))
    )
  }

  "tuple splits" in {
    p("(a, b) := x;") should parseAs (
      ImpSplitTuple(Seq(ImpVar("a"), ImpVar("b")), ImpVar("x"))
    )
    p("a, b := x;") should parseAs (
      ImpSplitTuple(Seq(ImpVar("a"), ImpVar("b")), ImpVar("x"))
    )
  }

  "function calls" in {
    p("z := f(x, y);") should parseAs (
      ImpSet(ImpVar("z"), ImpCall(ImpVar("f"), Seq(ImpVar("x"), ImpVar("y"))))
    )
  }

  "calls without assignment" in {
    p("f(x, y);") should parseAs (
      ImpCall(ImpVar("f"), Seq(ImpVar("x"), ImpVar("y")))
    )
  }

  "nested calls" in {
    p("f(x, g(y, z));") should parseAs (
      ImpCall(ImpVar("f"), Seq(ImpVar("x"), ImpCall(ImpVar("g"), Seq(ImpVar("y"), ImpVar("z")))))
    )
  }

  "dot call syntax" in {
    p("x := a.f(x, y);") should parseAs (
      ImpSet(ImpVar("x"), ImpCall(ImpVar("f"), Seq(ImpVar("a"), ImpVar("x"), ImpVar("y"))))
    )
  }

  "dot call syntax without assignment" in {
    p("a.f(x, y);") should parseAs (
      ImpCall(ImpVar("f"), Seq(ImpVar("a"), ImpVar("x"), ImpVar("y")))
    )
  }

  "borrowing calls" in {
    p("f(@x, y);") should parseAs (
      ImpCall(ImpVar("f"), Seq(ImpBorrowedArg(ImpVar("x")), ImpVar("y")))
    )
  }

  "borrowing dot syntax" in {
    p("@a.f(x, @y);") should parseAs (
      ImpCall(ImpVar("f"), Seq(ImpBorrowedArg(ImpVar("a")), ImpVar("x"), ImpBorrowedArg(ImpVar("y"))))
    )
  }

  "borrowing arg not a variable" in {
    p("f(@g(a));") should not be ('successful)
    p("f(@a + b);") should not be ('successful)
  }

  "if statements" in {
    p("if x then { return y; }") should parseAs (
      ImpIf(ImpVar("x"), ImpReturn(ImpVar("y")), ImpBlock(Seq.empty))
    )
    p("if x then { return y; } else { return 5; }") should parseAs (
      ImpIf(ImpVar("x"), ImpReturn(ImpVar("y")), ImpReturn(ImpInt(5)))
    )
    p("if x > 3 then { return y; } else { return 5; }") should parseAs (
      ImpIf(ImpCall(ImpVar(">"), Seq(ImpVar("x"), ImpInt(3))), ImpReturn(ImpVar("y")), ImpReturn(ImpInt(5)))
    )
  }
  // Ought to check ambiguous nested ifs too but it's not that important for a prototype

  "blocks" in {
    p("{ x := 1 + y; return x + 2; }") should parseAs (
      ImpBlock(Seq(
        ImpSet(ImpVar("x"), ImpCall(ImpVar("+"), Seq(ImpInt(1), ImpVar("y")))),
        ImpReturn(ImpCall(ImpVar("+"), Seq(ImpVar("x"), ImpInt(2))))
      ))
    )
  }

  "nested blocks" in {
    p("{ { } }") should parseAs (
      ImpBlock(Seq()) // It eliminates the outer block
    )

    // Should require no semicolon before `y := 2`.
    p("{ x := 1; if true then { } else { } y := 2; }") should parseAs (
      ImpBlock(Seq(
        ImpSet(ImpVar("x"), ImpInt(1)),
        ImpIf(ImpBool(true), ImpBlock(Seq()), ImpBlock(Seq())),
        ImpSet(ImpVar("y"), ImpInt(2))
      ))
    )
  }
}