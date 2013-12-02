package uniic.stdlib

import uniic.types._
import uniic.grs._

trait ScalaTypeConversions {

  /** A conversion from Scala to a GRS node and back. */
  case class Conversion[T](
    toNode: T => GrsNode,
    fromNode: PartialFunction[GrsNode, T]
  )

  /** Shorthand for GrsNode, for pattern-matching */
  final val N = GrsNode

  implicit val intConversion: Conversion[Int] = new Conversion[Int](
    toNode = { n => GrsInt(n).toNode },
    fromNode = { case N(GrsInt(n)) => n }
  )

  implicit val boolConversion: Conversion[Boolean] = new Conversion[Boolean](
    toNode = { v => GrsBool(v).toNode },
    fromNode = { case N(GrsBool(v)) => v }
  )

  implicit def tuple2Conversion[T, U](implicit tc: Conversion[T], tu: Conversion[U]): Conversion[(T, U)] = new Conversion[(T, U)](
    toNode = { case (a, b) => GrsTuple(2).toNode(Seq(tc.toNode(a), tu.toNode(b))) },
    fromNode = { case n@N(GrsTuple(2)) => (tc.fromNode(n.children(0)), tu.fromNode(n.children(1))) }
  )


  /** Ints, booleans and units can be given a base type automatically,
    * so we don't have to write out the type of e.g. simple arithmetic functions.
    */
  case class SimpleTyping[T](
    ty: BaseType
  )

  implicit val intSimpleTyping = new SimpleTyping[Int](TInt)
  implicit val boolSimpleTyping = new SimpleTyping[Boolean](TBool)
  implicit val unitSimpleTyping = new SimpleTyping[Unit](TUnit)
}
