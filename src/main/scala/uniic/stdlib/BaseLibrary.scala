package uniic.stdlib

import uniic.parsers.Associativity
import uniic.types._
import uniic.grs._

trait BaseLibrary extends LibraryProvider {
  import Associativity._
  import TypeDSL._

  addTypes(
    "Int" -> TInt,
    "Bool" -> TBool,
    "Unit" -> TUnit
  )

  val defaultOperatorSet = Map(
    "**" -> (8, Right),
    "*" -> (7, Left),
    "/" -> (7, Left),
    "%" -> (7, Left),
    "+" -> (6, Left),
    "-" -> (6, Left),
    "<" -> (4, NoAssoc),
    ">" -> (4, NoAssoc),
    "<=" -> (4, NoAssoc),
    ">=" -> (4, NoAssoc),
    "==" -> (4, NoAssoc),
    "!=" -> (4, NoAssoc)
  )

  fun2Simple("+", (x: Int, y: Int) => x + y)
  fun2Simple("-", (x: Int, y: Int) => x - y)
  fun2Simple("*", (x: Int, y: Int) => x * y)
  fun2Simple("/", (x: Int, y: Int) => x / y)
  fun2Simple("%", (x: Int, y: Int) => x % y)

  fun2Simple("<", (x: Int, y: Int) => x < y)
  fun2Simple(">", (x: Int, y: Int) => x > y)
  fun2Simple("<=", (x: Int, y: Int) => x <= y)
  fun2Simple(">=", (x: Int, y: Int) => x >= y)
  fun2Simple("==", (x: Int, y: Int) => x == y)
  fun2Simple("!=", (x: Int, y: Int) => x != y)

  fun1("not", TypeSchema(TBool.nonUniq :-(ANonUniq)> TBool.nonUniq), { v: Boolean => !v })

  // We don't have type ascription yet, but we can use helper functions like this
  fun("requireUnique", (TVar("t").uniq :-(ANonUniq)> TVar("t").uniq).generalize(Set.empty), { case Seq(x) => x })

  fun("gen", (TVar("t").uniq :-(ANonUniq)> TVar("t")(AVar("a"))).generalize(Set.empty), { case Seq(x) => x })

  fun("consume", (TVar("t").uniq :-(ANonUniq)> TUnit.nonUniq).generalize(Set.empty), { case Seq(_) => GrsUnit.toNode })

  fun("makeShared", (TVar("t").uniq :-(ANonUniq)> TVar("t").nonUniq).generalize(Set.empty), { case Seq(x) => x })

  fun("identity", (TVar("t")(AVar("a")) :-(ANonUniq)> TVar("t")(AVar("a"))).generalize(Set.empty), { case Seq(x) => x })
  fun("rename", (TVar("t")(AVar("a")) :-(ANonUniq)> TVar("t")(AVar("a"))).generalize(Set.empty), { case Seq(x) => x })
}
