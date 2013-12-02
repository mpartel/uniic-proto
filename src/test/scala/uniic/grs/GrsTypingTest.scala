package uniic.grs
import uniic.fun._
import uniic.fun.analysis.FunTree
import uniic.types._
import uniic.misc._
import uniic.stdlib._
import uniic.parsers.LangParsers
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterEach

class GrsTypingTest
  extends FreeSpec
  with ShouldMatchers
  with TypeSchemaAssertions
  with BeforeAndAfterEach
{
  var symTab = Stdlib.symTab
  var typeSymTab = Stdlib.typeSymTab
  var drawTypingAs = Option.empty[String]

  override def afterEach() {
    super.afterEach()
    symTab = Stdlib.symTab
    typeSymTab = Stdlib.typeSymTab
    drawTypingAs = None
  }

  def checkEq(funExprStr: String, typeExprStr: String) {
    val (expected, actual) = getExpectedAndActual(funExprStr, typeExprStr)
    actual should beEquivalentTo (expected)
  }

  def checkInst(funExprStr: String, typeExprStr: String) {
    val (expected, actual) = getExpectedAndActual(funExprStr, typeExprStr)
    expected should beInstantiatableFrom (actual)
  }

  def getExpectedAndActual(funExprStr: String, typeExprStr: String): (TypeSchema, TypeSchema) = {
    val expected = parseTypeSchema(typeExprStr)
    val actual = inferType(makeGrs(funExprStr))
    (expected, actual)
  }

  // TODO: extract these helpers into some superclass. Similar stuff is useful in integration tests.
  def parseTypeSchema(s: String): TypeSchema = {
    TypeEval.evalTypeSchema(typeSymTab, parseTypeExpr(s))
  }

  def parseKinded(s: String): Kinded = {
    TypeEval.evalKinded(typeSymTab, parseTypeExpr(s))
  }

  def parseTypeExpr(s: String): TypeExpr = {
    LangParsers.parseTypeExpr(s) match {
      case Right(te) => te
      case Left(error) => throw new CompilerException(error)
    }
  }

  def makeGrs(s: String): Grs = {
    val funExpr = LangParsers.parseFunExprOrThrow(s, Stdlib.defaultOperatorSet)
    val funTree = new FunTree(funExpr)
    FunToGrs(funTree, Map.empty)
  }

  def inferType(grs: Grs): TypeSchema = {
    val rigidVars = Set.empty[String]
    val typing = GrsTyping.applyAndUnify(
      grs,
      symTab.mapValues(_.nonborrowingType),
      typeSymTab
    )
    drawTypingAs.foreach { name =>
      GrsTyping.typingToGraphviz(grs, typing).setName(name).writeImageToDebugDir()
    }
    typing(grs.rootNode).generalize(rigidVars)
  }

  "constants" in {
    checkEq("3", "Int")
    checkEq("true", "Bool")
    checkEq("()", "Unit")
  }

  "simple builtin function calls" in {
    checkEq("+", "(Int, Int) -> Int")
    checkEq("1 + 1", "Int")
    checkEq("1 + 2 * 3", "Int")
    evaluating { inferType(makeGrs("1 + true")) } should produce [TypeError]
  }

  "identity function" in {
    checkEq("\\x. x", "forall t:T a:A fa:A. t^a -fa> t^a")
    checkInst("\\x. x", "forall t:T a:A. t^a -> t^a")
  }

  "simple functions" in {
    checkInst("\\x. x + x", "Int -> Int")
    checkInst("\\x, y. x + y", "(Int, Int) -> Int")
    checkInst("\\x. \\y. x + y", "Int -> Int -> Int")
  }

  "type-annotated functions" in {
    checkEq("\\x: IntArray*. x", "forall uf:A. IntArray* -(uf)> IntArray*")
    checkEq("\\x: IntArray^u. x", "forall u:A uf:A. IntArray^u -(uf)> IntArray^u")
    checkEq("\\x: T^u. x", "forall T:T u:A uf:A. T^u -(uf)> T^u")
  }

  "simple lambda applications" in {
    checkEq("(\\x, y. x)(1, 2)", "Int")
    checkEq("(\\x. \\y. x)(1)(2)", "Int")
  }

  "calling identity function preserves uniqueness" in {
    checkEq("(\\x.x)(mkIntArray(5))", "forall u:A. IntArray^u")
    checkInst("(\\x.x)(mkIntArray(5))", "IntArray*")
  }

  "returning functions" in {
    checkEq(
      "let f = \\x. \\y. x + y in f(3)",
      "forall fa:A. Int -(fa)> Int"
    )
  }

  "if-then-else" in {
    checkEq("if true then 1 else 2", "Int")
  }

  "tuples" in {
    checkEq("\\x, y. (x, y)", """forall t1:T t2:T u1:A u2:A u3:A uf:A. (t1^u1, t2^u2) -uf> (t1^u1, t2^u2)^(u1\/u2\/u3)""")
  }

  "tuple with unique member" in {
    checkEq("(1, mkIntArray(5))", """forall ua:A ut:A. (Int, IntArray^ua)^(ut\/ua)""")
  }

  "pattern matching" in {
    checkEq("\\x. x match { case (a, b) => a + b }", """forall ut:A uf:A. ((Int, Int)^ut) -uf> Int""")
  }

  "passing tuple with unique through identity" in {
    checkEq("identity((1, mkIntArray(5)))", """forall ua:A ut:A. (Int, IntArray^ua)^(ut\/ua)""")
  }

  "let-binding tuple with unique" in {
    checkEq("let x = (1, mkIntArray(5)) in x", """forall ua:A ut:A. (Int, IntArray^ua)^(ut\/ua)""")
  }

  "the const function" in {
    val expectedTy = "forall t1:T t2:T u1:A u2:A uf1:A uf2:A. t1^u1 -(uf1)> t2^u2 -(uf2|u1)> t1^u1"
    checkEq("""\a. \b. a""", expectedTy)
    checkEq("""let f = \x. \y. x in \a. f(a)""", expectedTy)
  }

  "function with potential unique in closure must have the appropriate closure attribute" in {
    checkEq(
      "let f = \\x. \\y. x in f(mkIntArray(10))",
      "forall t:T ut:A uf:A ua:A. t^ut -(uf|ua)> IntArray^ua"
    )
  }

  "using an explicitly typed generic function" in {
    checkEq("let f = \\x: T*. x in f(mkIntArray(5))", "IntArray*")
    checkEq("let f = \\x: T*. gen(x) in f(mkIntArray(5))", "forall u:A. IntArray^u")
  }

  "explicitly typed generic function with name like autogenerated type var" in {
    for (i <- 0 to 20) {
      val a = VarStream.forAVars(Set.empty).apply(i)
      val t = VarStream.forTVars(Set.empty).apply(i)
      val code = s"let f = \\x: ${t}^${a}. x in f(f(requireUnique(mkIntArray(3))))"
      checkEq(code, "IntArray*")
    }
  }

  val expectedDupType = "forall t:T uf:A ut:A. t -(uf)> (t, t)^ut"

  "simple dup" in {
    checkEq("\\x. (x, x)", expectedDupType)
  }

  "dup going through multiple let aliases" in {
    checkEq("\\x. let z = y and y = x in (y, z)", expectedDupType)
    checkEq("\\x. let z = y and y = (x, x) in z", expectedDupType)
  }

  "a slightly complicated dup" in {
    checkEq("\\x. let f = \\y. (x, x) in f(8)", expectedDupType)
  }

  "single use of a function with an unique closure" in {
    checkEq("\\f : (Int -(*|*)> Int). f(1)", "forall uf:A. (Int -(*|*)> Int) -(uf)> Int")
  }

  "multiple uses of a function with an unique closure" in {
    inferType(makeGrs("\\f : (Int -(|*)> Int). (f, f)")) // Is fine since we don't call it
    evaluating { inferType(makeGrs("\\f : (Int -(*|*)> Int). (f(1), f(2))")) } should produce [TypeError]
    evaluating { inferType(makeGrs("\\f : (Int -(|*)> Int). (f(1), f(2))")) } should produce [TypeError]
    evaluating { inferType(makeGrs("\\f : (Int^u -(|~u)> Int). (f(1), f(2))")) } should produce [TypeError]

    val ex = evaluating {
      inferType(makeGrs("""
        let f = \a : IntArray*. \x. a
        and g = f(mkIntArray(5))
        in (g(1), g(2))
      """))
    } should produce [TypeError]
  }

  "dup using closure trick" in {
    // This is "sneakyDup" from de Vries' PhD.
    checkEq("""
      let const = \a. \b. a
      in
        \x.
          let f = \y. const(x)(y)
          in (f(1), f(2))
    """, expectedDupType)
  }

  "`fst p = p match { case (x, y) -> x }`" in {
    checkEq(
      "\\p. p match { case (x, y) => x }",
      """forall u:A v:A w:A uf:A t:T s:T. ((t^u, s^v)^(u \/ v \/ w)) -(uf)> t^u"""
    )
  }

  "`snd p = p match { (x, y) -> y }`" in {
    checkEq(
      "\\p. p match { case (x, y) => y }",
      """forall u:A v:A w:A uf:A t:T s:T. ((t^u, s^v)^(u \/ v \/ w)) -(uf)> s^v"""
    )
  }

  "pattern matching on an unique" in {
    checkEq("""
      (5, mkIntArray(3)) match {
        case (x, y) => y
      }
    """, "forall u:A. IntArray^u")
  }

  "factorial" in {
    val fact = """
      let fact = \n.
        if n <= 1
          then 1
          else n * fact(n - 1)
      in fact
      """
    checkEq(fact, "Int -> Int")
  }

  "mutual recursion" in {
    val evenOdd = """
      let even = \n. if n == 0 then true else odd(n - 1)
      and odd = \n. if n == 0 then false else even(n - 1)
      in (even, odd)
      """
    val expectedType = "forall u:A. (Int -> Bool, Int -> Bool)^u"
    checkEq(evenOdd, expectedType)
  }

  "applying stdlib function translated from a borrowing function" in {
    val expr = """
      let a = mkIntArray(3)
      in intArraySet(a, 2, 5)
      """
    checkEq(expr, """forall u1:A u2:A. ((), IntArray*)*""")
  }

  "passing an unique as a non-unique parameter" in {
    val expr = """
      let f = \x. (x, x)
      and a = mkIntArray(3)
      in f(a)
    """
    checkEq(expr, "forall u:A. (IntArray, IntArray)^u")
  }

  "explicitly generalizing unique to anyattr" in {
    val expr1 = """
      let a = mkIntArray(3)
      and f = \x:T*. x
      in f(f(f(a)))
    """
    checkEq(expr1, "IntArray*") // (unfortunately)

    val expr2 = """
      let a = mkIntArray(3)
      and f = \x:T*. x
      in gen(f(f(f(a))))
    """
    checkEq(expr2, "forall u:A. IntArray^u")
  }
}
