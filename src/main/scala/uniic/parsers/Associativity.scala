package uniic.parsers

sealed trait Associativity

object Associativity {
  object Left extends Associativity
  object Right extends Associativity
  object NoAssoc extends Associativity
}
