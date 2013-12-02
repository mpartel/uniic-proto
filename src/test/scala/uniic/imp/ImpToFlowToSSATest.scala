package uniic.imp
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.flow._
import uniic.types._
import uniic.stdlib.Stdlib
import uniic.test._

class ImpToFlowToSSATest extends FreeSpec with ShouldMatchers with ImpTestUtils with FlowTestUtils with TypeMatchers {
  // Use the following snippet to draw these for debugging:
  // FlowGraph.fromStatements(result).toGraphviz.writeImageToDebugDir()

  "simple assignments" in {
    val result = toSsa("""
      {
        x := 1;
        x := x + 2;
        x := x + 5;
        y := x;
      }
    """)
    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('x, ImpInt(1)),
      FlowSetConst('$arg1$0, ImpInt(2)),
      FlowCall('x|1, '+, Seq('x, '$arg1$0)),
      FlowSetConst('$arg1$1, ImpInt(5)),
      FlowCall('x|2, '+, Seq('x|1, '$arg1$1)),
      FlowSet('y, 'x|2),
      FlowSetConst('$defaultReturn, ImpUnit),
      FlowReturn('$defaultReturn)
    )))
  }

  "if statements" in {
    val result = toSsa("""
      {
        x := 1;
        x := x + 2;
        if x > 1 then {
          y := x;
        } else {
          y := -3;
        }
        return y;
      }
    """)
    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('x, ImpInt(1)),
      FlowSetConst('$arg1$0, ImpInt(2)),
      FlowCall('x|1, '+, Seq('x, '$arg1$0)),
      FlowSetConst('$arg1$1, ImpInt(1)),
      FlowCall('$ifHead$0, '>, Seq('x|1, '$arg1$1)),
      FlowGotoCond('$ifHead$0, FlowLabel("then0"), FlowLabel("else0")),

      FlowLabel("then0"),
      FlowSet('y, 'x|1),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("else0"),
      FlowSetConst('y|1, ImpInt(-3)),
      FlowGoto(FlowLabel("ifEnd0")),
      FlowLabel("ifEnd0"),

      FlowPhiStmt('y|2, Seq('y, 'y|1)),
      FlowSet('$returnValue$0, 'y|2),
      FlowReturn('$returnValue$0)
    )))
  }

  "if with return in both branches" in {
    val result = toSsa("""
      if 1 < 2 then { return 1; } else { return 2; }
    """)
    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('$arg0$0, ImpInt(1)),
      FlowSetConst('$arg1$0, ImpInt(2)),
      FlowCall('$ifHead$0, '<, Seq('$arg0$0, '$arg1$0)),
      FlowGotoCond('$ifHead$0, FlowLabel("then0"), FlowLabel("else0")),

      FlowLabel("then0"),
      FlowSetConst('$returnValue$0, ImpInt(1)),
      FlowReturn('$returnValue$0),

      FlowLabel("else0"),
      FlowSetConst('$returnValue$1, ImpInt(2)),
      FlowReturn('$returnValue$1)
    )))
  }

  "while statements" in {
    val result = toSsa("""
      {
        x := 123;
        while x > 5 do {
          x := x / 2;
        }
        return x;
      }
    """)
    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('x, ImpInt(123)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileHead0"),
      FlowPhiStmt('x|2, Seq('x, 'x|1)),
      FlowSetConst('$arg1$0, ImpInt(5)),
      FlowCall('$whileCondition$0, '>, Seq('x|2, '$arg1$0)),
      FlowGotoCond('$whileCondition$0, FlowLabel("whileBody0"), FlowLabel("whileEnd0")),

      FlowLabel("whileBody0"),
      FlowSetConst('$arg1$1, ImpInt(2)),
      FlowCall('x|1, '/, Seq('x|2, '$arg1$1)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileEnd0"),
      FlowSet('$returnValue$0, 'x|2),
      FlowReturn('$returnValue$0)
    )))
  }

  "return inside while" in {
    val result = toSsa("""
      {
        x := 123;
        while x > 5 do {
          return x;
        }
        return 0;
      }
    """)
    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('x, ImpInt(123)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileHead0"),
      FlowSetConst('$arg1$0, ImpInt(5)),
      FlowCall('$whileCondition$0, '>, Seq('x, '$arg1$0)),
      FlowGotoCond('$whileCondition$0, FlowLabel("whileBody0"), FlowLabel("whileEnd0")),

      FlowLabel("whileBody0"),
      FlowSet('$returnValue$0, 'x),
      FlowReturn('$returnValue$0),

      FlowLabel("whileEnd0"),
      FlowSetConst('$returnValue$1, ImpInt(0)),
      FlowReturn('$returnValue$1)
    )))
  }

  "the example from \"SSA is Functional Programming\" with an if-else in a while" in {
    val result = toSsa("""
      {
        i := 1;
        j := 1;
        k := 0;
        while k < 100 do {
          if j < 20 then {
            j := i;
            k := k + 1;
          } else {
            j := k;
            k := k + 2;
          }
        }
        return j;
      }
    """)

    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('i, ImpInt(1)),
      FlowSetConst('j, ImpInt(1)),
      FlowSetConst('k, ImpInt(0)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileHead0"),
      FlowPhiStmt('j|4, Seq('j, 'j|3)),
      FlowPhiStmt('k|4, Seq('k, 'k|3)),
      FlowSetConst('$arg1$0, ImpInt(100)),
      FlowCall('$whileCondition$0, '<, Seq('k|4, '$arg1$0)),
      FlowGotoCond('$whileCondition$0, FlowLabel("whileBody0"), FlowLabel("whileEnd0")),

      FlowLabel("whileBody0"),
      FlowSetConst('$arg1$1, ImpInt(20)),
      FlowCall('$ifHead$0, '<, Seq('j|4, '$arg1$1)),
      FlowGotoCond('$ifHead$0, FlowLabel("then0"), FlowLabel("else0")),

      FlowLabel("then0"),
      FlowSet('j|1, 'i),
      FlowSetConst('$arg1$2, ImpInt(1)),
      FlowCall('k|1, '+, Seq('k|4, '$arg1$2)),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("else0"),
      FlowSet('j|2, 'k|4),
      FlowSetConst('$arg1$3, ImpInt(2)),
      FlowCall('k|2, '+, Seq('k|4, '$arg1$3)),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("ifEnd0"),
      FlowPhiStmt('j|3, Seq('j|1, 'j|2)),
      FlowPhiStmt('k|3, Seq('k|1, 'k|2)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileEnd0"),
      FlowSet('$returnValue$0, 'j|4),
      FlowReturn('$returnValue$0)
    )))
  }

  "nested loops and ifs" in {
    val result = toSsa("""
      {
        x := 0;
        y := 0;
        while x < 100 do {
          if x % 6 == 0 then {
            while y < x * 100 do {
              y := y * 10;
            }
          }
          x := x / 2;
        }
        return y;
      }
    """)
    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('x, ImpInt(0)),
      FlowSetConst('y, ImpInt(0)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileHead0"),
      FlowPhiStmt('x|2, Seq('x, 'x|1)),
      FlowPhiStmt('y|4, Seq('y, 'y|3)),
      FlowSetConst('$arg1$0, ImpInt(100)),
      FlowCall('$whileCondition$0, '<, Seq('x|2, '$arg1$0)),
      FlowGotoCond('$whileCondition$0, FlowLabel("whileBody0"), FlowLabel("whileEnd0")),

      FlowLabel("whileBody0"),
      FlowSetConst('$arg1$1, ImpInt(6)),
      FlowCall('$arg0$0, '%, Seq('x|2, '$arg1$1)),
      FlowSetConst('$arg1$2, ImpInt(0)),
      FlowCall('$ifHead$0, '==, Seq('$arg0$0, '$arg1$2)),
      FlowGotoCond('$ifHead$0, FlowLabel("then0"), FlowLabel("else0")),

      FlowLabel("then0"),
      FlowGoto(FlowLabel("whileHead1")),

      FlowLabel("whileHead1"),
      FlowPhiStmt('y|2, Seq('y|1, 'y|4)),
      FlowSetConst('$arg1$4, ImpInt(100)),
      FlowCall('$arg1$3, '*, Seq('x|2, '$arg1$4)),
      FlowCall('$whileCondition$1, '<, Seq('y|2, '$arg1$3)),
      FlowGotoCond('$whileCondition$1, FlowLabel("whileBody1"), FlowLabel("whileEnd1")),

      FlowLabel("whileBody1"),
      FlowSetConst('$arg1$5, ImpInt(10)),
      FlowCall('y|1, '*, Seq('y|2, '$arg1$5)),
      FlowGoto(FlowLabel("whileHead1")),

      FlowLabel("whileEnd1"),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("else0"),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("ifEnd0"),
      FlowPhiStmt('y|3, Seq('y|2, 'y|4)),
      FlowSetConst('$arg1$6, ImpInt(2)),
      FlowCall('x|1, '/, Seq('x|2, '$arg1$6)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileEnd0"),
      FlowSet('$returnValue$0, 'y|4),
      FlowReturn('$returnValue$0)
    )))
  }

  "nested ifs" in {
    val result = toSsa("""
      {
        a := 1;
        if true then {
          if true then {
            a := 2;
          } else {
            a := 3;
          }
        } else {
          a := 4;
        }
        return a;
      }
    """)

    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('a, ImpInt(1)),
      FlowSetConst('$ifHead$0, ImpBool(true)),
      FlowGotoCond('$ifHead$0, FlowLabel("then0"), FlowLabel("else0")),

      FlowLabel("then0"),
      FlowSetConst('$ifHead$1, ImpBool(true)),
      FlowGotoCond('$ifHead$1, FlowLabel("then1"), FlowLabel("else1")),

      FlowLabel("then1"),
      FlowSetConst('a|1, ImpInt(2)),
      FlowGoto(FlowLabel("ifEnd1")),

      FlowLabel("else1"),
      FlowSetConst('a|2, ImpInt(3)),
      FlowGoto(FlowLabel("ifEnd1")),

      FlowLabel("ifEnd1"),
      FlowPhiStmt('a|4, Seq('a|1, 'a|2)),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("else0"),
      FlowSetConst('a|3, ImpInt(4)),
      FlowGoto(FlowLabel("ifEnd0")),

      FlowLabel("ifEnd0"),
      FlowPhiStmt('a|5, Seq('a|3, 'a|4)),
      FlowSet('$returnValue$0, 'a|5),
      FlowReturn('$returnValue$0)
    )))
  }

  "init of var in a loop with another init after the loop" in {
    val result = toSsa("""
      {
        a := 0;
        while a < 10 do {
          x := a + 1;
          a := x;
        }
        x := a;
        return x;
      }
    """)

    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('a, ImpInt(0)),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileHead0"),
      FlowPhiStmt('a|2, Seq('a, 'a|1)),
      FlowSetConst('$arg1$0, ImpInt(10)),
      FlowCall('$whileCondition$0, '<, Seq('a|2, '$arg1$0)),
      FlowGotoCond('$whileCondition$0, FlowLabel("whileBody0"), FlowLabel("whileEnd0")),

      FlowLabel("whileBody0"),
      FlowSetConst('$arg1$1, ImpInt(1)),
      FlowCall('x, '+, Seq('a|2, '$arg1$1)),
      FlowSet('a|1, 'x),
      FlowGoto(FlowLabel("whileHead0")),

      FlowLabel("whileEnd0"),
      FlowSet('x|1, 'a|2),
      FlowSet('$returnValue$0, 'x|1),
      FlowReturn('$returnValue$0)
    )))
  }

  "borrowing" in {
    val result = toSsa("""
      {
        a := 0;
        x := f(@a);
        return x;
      }
    """)

    result should be (equivalentTo(Seq(
      FlowLabel("_start"),
      FlowSetConst('a, ImpInt(0)),
      FlowCall('x, 'f, Seq('a @@ ('a|1))),
      FlowSet('$returnValue$0, 'x),
      FlowReturn('$returnValue$0)
    )))
  }
}
