package uniic.stdlib

import uniic.grs._
import uniic.types._
import uniic.module._

class LibraryProvider extends ScalaTypeConversions {
  private var _symTab = Map.empty[String, Stage.SymTabEntry]
  private var _typeSymTab = Map.empty[String, Kinded]

  protected def fun2Simple[T1, T2, R]
      (name: String, f: (T1, T2) => R)
      (implicit cT1: Conversion[T1], cT2: Conversion[T2], cR: Conversion[R],
                 stT1: SimpleTyping[T1], stT2: SimpleTyping[T2], stR: SimpleTyping[R]) {

    val params = Seq(
      TFunParam(stT1.ty.nonUniq, MNone),
      TFunParam(stT2.ty.nonUniq, MNone)
    )
    val returnType = stR.ty.nonUniq
    val typeSchema = TFun(params, ANonUniq, returnType).nonUniq.toTrivialTypeSchema
    fun2(name, typeSchema, f)
  }

  protected def fun1[T, R]
      (name: String, ts: TypeSchema, f: (T) => R)
      (implicit cT: Conversion[T], cR: Conversion[R]){
    val impl = { args: Seq[GrsNode] =>
      val x = cT.fromNode(args(0))
      cR.toNode(f(x))
    }
    fun(name, ts, impl)
  }

  protected def fun2[T1, T2, R]
      (name: String, ts: TypeSchema, f: (T1, T2) => R)
      (implicit cT1: Conversion[T1], cT2: Conversion[T2], cR: Conversion[R]) {
    val impl = { args: Seq[GrsNode] =>
      val arg1 = cT1.fromNode(args(0))
      val arg2 = cT2.fromNode(args(1))
      cR.toNode(f(arg1, arg2))
    }
    fun(name, ts, impl)
  }

    protected def fun3[T1, T2, T3, R]
      (name: String, ts: TypeSchema, f: (T1, T2, T3) => R)
      (implicit cT1: Conversion[T1], cT2: Conversion[T2], cT3: Conversion[T3], cR: Conversion[R]) {
    val impl = { args: Seq[GrsNode] =>
      val arg1 = cT1.fromNode(args(0))
      val arg2 = cT2.fromNode(args(1))
      val arg3 = cT3.fromNode(args(2))
      cR.toNode(f(arg1, arg2, arg3))
    }
    fun(name, ts, impl)
  }

  private def defaultParamNames(ts: TypeSchema): Seq[String] = {
    ts.ty.baseType match {
      case TFun(paramTys, _, _) => paramTys.indices.map(i => "p" + i)
      case _ => Seq.empty
    }
  }

  protected def fun(
    name: String,
    ts: TypeSchema,
    impl: (Seq[GrsNode] => GrsNode)
  ) {
    fun(name, ts, defaultParamNames(ts), impl)
  }

  protected def fun(
    name: String,
    ts: TypeSchema,
    paramNames: Seq[String],
    impl: (Seq[GrsNode] => GrsNode)
  ) {
    val f = GrsBuiltinFunction(name, paramNames, impl)
    _symTab += (name -> Stage.SymTabEntry(f, ts, TypeBorrowingRemoval(ts)))
  }

  protected def alias(original: String, newName: String) {
    _symTab += (newName -> _symTab(original))
  }

  protected def addType(name: String, t: Kinded) {
    _typeSymTab += (name -> t)
  }

  protected def addTypes(defs: (String, Kinded)*) {
    _typeSymTab ++= (defs)
  }

  def symTab: Map[String, Stage.SymTabEntry] = _symTab
  def symTabWithoutTypes: Map[String, GrsValue] = _symTab.mapValues(_.value)

  def typeSymTab: Map[String, Kinded] = _typeSymTab
}
