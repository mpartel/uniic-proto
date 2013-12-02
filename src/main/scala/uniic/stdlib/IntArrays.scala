package uniic.stdlib

import uniic.types._
import uniic.grs._

trait IntArrays extends LibraryProvider { self: BaseLibrary =>
  import TypeDSL._

  val intArrayTy = BuiltinType("IntArray")

  addTypes(
    "IntArray" -> intArrayTy
  )

  implicit val intArrayConversion: Conversion[Array[Int]] = new Conversion[Array[Int]](
    { case a: Array[Int] => GrsBuiltinValue(a, Some(intArrayTy)).toNode },
    { case N(GrsBuiltinValue(v, Some(BuiltinType("IntArray")))) => v.asInstanceOf[Array[Int]] }
  )

  private val a = AVar("a")
  private val b = AVar("b")

  fun1(
    "mkIntArray",
    TypeSchema(Seq(a), TInt.nonUniq :-(ANonUniq)> intArrayTy.withAttr(a)),
    { (sz: Int) => new Array[Int](sz) }
  )

  fun2(
    "intArrayGet",
    TypeSchema(Seq(a), Seq(intArrayTy.withAttr(a).borrowed, TInt.nonUniq.param) :-(ANonUniq)> TInt.nonUniq),
    { (a: Array[Int], i: Int) => (a(i), a) }
  )

  fun1(
    "intArrayLength",
    TypeSchema(Seq(a), intArrayTy.withAttr(a).borrowed:-(ANonUniq)> TInt.nonUniq),
    { a: Array[Int] => (a.length, a) }
  )

  fun(
    "intArraySet",
    TypeSchema(Seq(intArrayTy.uniq.borrowed, TInt.nonUniq.param, TInt.nonUniq.param) :-(ANonUniq)> TUnit.nonUniq),
    {
      case Seq(an@N(GrsBuiltinValue(a: Array[Int], _)), N(GrsInt(i)), N(GrsInt(x))) => {
        a(i) = x
        GrsTuple(2).toNode(Seq(GrsUnit.toNode, an))
      }
    }
  )

  fun(
    "intArraySetNoBorrow",
    TypeSchema(Seq(a), Seq(intArrayTy.uniq.param, TInt.nonUniq.param, TInt.nonUniq.param) :-(ANonUniq)> intArrayTy.withAttr(a)),
    {
      case Seq(an@N(GrsBuiltinValue(a: Array[Int], _)), N(GrsInt(i)), N(GrsInt(x))) => {
        a(i) = x
        an
      }
    }
  )

  fun(
    "intArrayGetNoBorrow",
    TypeSchema(Seq(a),
      Seq(intArrayTy.withAttr(a).param, TInt.nonUniq.param)
        :-(ANonUniq)>
        TTuple(Seq(TInt.nonUniq, intArrayTy.withAttr(a))).withAttr(a \/ b)
    ),
    {
      case Seq(an@N(GrsBuiltinValue(a: Array[Int], _)), N(GrsInt(i))) => {
        GrsTuple(2).toNode(Seq(GrsInt(a(i)).toNode, an))
      }
    }
  )

  fun(
    "intArrayLengthNoBorrow",
    TypeSchema(Seq(a),
      intArrayTy.withAttr(a).param
        :-(ANonUniq)>
        TTuple(Seq(TInt.nonUniq, intArrayTy.withAttr(a))).withAttr(a \/ b)
    ),
    {
      case Seq(an@N(GrsBuiltinValue(a: Array[Int], _))) => {
        GrsTuple(2).toNode(Seq(GrsInt(a.length).toNode, an))
      }
    }
  )
}
