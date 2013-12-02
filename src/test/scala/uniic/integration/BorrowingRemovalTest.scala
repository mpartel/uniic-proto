package uniic.integration

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.types._
import uniic.grs._
import uniic.misc._

class BorrowingRemovalTest extends FreeSpec with ShouldMatchers with ModuleLevelItBase {

  "int array set twice" in {
    """
    module testcase
    def f(a: @IntArray*): Int {
      @a.intArraySet(3, 123);
      @a.intArraySet(3, 456);
      return @a.intArrayGet(3);
    }
    def main(): Int {
      a := mkIntArray(5);
      return f(@a);
    }
    """.typeOfMain should beEquivalentTo ("forall u:A. () -(u)> Int")
  }

  "fiboarray" in {
    val code =
      """
      module testcase
      def main(): Int {
        a := mkIntArray(15);
        @a.intArraySet(0, 0);
        @a.intArraySet(1, 1);
        i := 2;
        while i < @a.intArrayLength() do {
          @a.intArraySet(i, (@a.intArrayGet(i - 1) + @a.intArrayGet(i - 2)));
          i := i + 1;
        }
        return @a.intArrayGet(14);
      }
      """

    val program = new ImperativeProgram(code)
    program.typeOfMain should beEquivalentTo ("forall u:A. () -(u)> Int")
    program.callingMain should evaluateTo ("377")
  }

  "attempting to consume a borrowed" in {
    val code =
      """
      module testcase
      def foo(a: @IntArray*): Unit {
        consume(a);
        return ();
      }
      """
    val typeError = evaluating {
      compile(code)
    } should produce [TypeError]
    // Would be nice if we got a nicer error message than "cannot unify * =:= x".
    // e.g. "Borrowed variable `a` was consumed.".
  }

  "simple wrapper for intArraySet" in {
    val code =
      """
      module testcase
      def intArraySet2(a: @IntArray*, index: Int, value: Int): Unit {
        @a.intArraySet(index, value);
        return ();
      }
      def main(): Int {
        a := mkIntArray(3);
        @a.intArraySet2(1, 123);
        return @a.intArrayGet(1);
      }
      """
    val program = new ImperativeProgram(code)
    program.typeOfMain should beEquivalentTo ("forall u:A. () -(u)> Int")
    program.borrowingTypeOf("intArraySet2") should beEquivalentTo ("forall u:A. (@IntArray*, Int, Int) -(u)> Unit")
    program.finalTypeOf("intArraySet2") should beEquivalentTo ("""forall uf:A ua:A ut:A. (IntArray*, Int, Int) -(uf)> (Unit, IntArray*)*""")
    program.callingMain should evaluateTo ("123")
  }

  // FIXME
  "borrowing tuples" is pending /*in {
debug = true
    val code =
      """
      module testcase
      def f(t: @(Int, IntArray^u)*): Int {
        (i, a) := t;
        r := @a.intArrayGet(i);
        t := (i + 1, a);
        return r;
      }
      def main(): Int {
        a := mkIntArray(3);
        @a.intArraySet(1, 123);
        t := (1, a);
        return f(@t);
      }
      """
    val program = new ImperativeProgram(code)
    program.finalTypeOf("f") should beEquivalentTo (
      """forall u:A uf:A. ((Int, IntArray^u)*) -(uf)> (Int, (Int, IntArray^u)*)*"""
    )
    program.typeOfMain should beEquivalentTo ("forall u:A. () -(u)> Int")
  }*/

  "taking borrowing function as a parameter" is pending /*in {
// FIXME: the complicated-looking attribute is not a problem since it's universally quantified.
// The return type's attribute is the problem. SSATyping gets it wrong.
// It must preserve the attribute from f. Otherwise, in the non-borrowing world, f could return a shared IntArray.
// (we cannot unfortunately demand an universally quantified f -- a weakness of this rank-1 version of De Vries' system)
//debug = true
    val code =
      """
      module testcase
      def foo(x: @IntArray*): Unit {
        x.intArraySet(0, 123);
        return ();
      }
      def withNewArray(f: ((@IntArray*) -> Unit)): IntArray* {
        a := mkIntArray(10);
        f(a);
        return a;
      }
      def main(): Int {
        a := withNewArray(foo);
        return a.intArrayGet(0);
      }
      """
    val program = new ImperativeProgram(code)
    program.typeOfMain should beEquivalentTo ("forall u:A. () -(u)> Int")
    program.borrowingTypeOf("withNewArray") should beEquivalentTo (
      """forall uf:A u:A. ((@IntArray*) -> Unit) -(uf)> IntArray^u"""
    )
    program.finalTypeOf("withNewArray") should beEquivalentTo (
      """forall uf:A u:A. ((IntArray*) -> (Unit, IntArray^u)) -(uf)> IntArray^u"""
    )
  }

  // TODO: a test like above, but with f being non-borrowing, and pass it a foo that returns a shared IntArray.
*/
}
