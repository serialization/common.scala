package ogss.common.scala.internal.fieldTypes

import ogss.common.scala.internal.FieldType
import ogss.common.streams.InStream
import ogss.common.streams.OutStream

/**
 * Integer types
 *
 * @author Timm Felden
 */
sealed abstract class IntegerType[T](_typeID : Int) extends FieldType[T](_typeID);

/**
 * @author Timm Felden
 */
case object I8 extends IntegerType[Byte](1) {

  override def r(in : InStream) = in.i8()

  override def w(target : Byte, out : OutStream) : Boolean = {
    val v = if (null == target) 0.toByte else target
    out.i8(v)
    return 0 == v
  }

  override def toString = "i8"
}

/**
 * @author Timm Felden
 */
case object I16 extends IntegerType[Short](2) {

  override def r(in : InStream) = in.i16()

  override def w(target : Short, out : OutStream) : Boolean = {
    val v = if (null == target) 0.toShort else target
    out.i16(v)
    return 0 == v
  }

  override def toString = "i16"
}

/**
 * @author Timm Felden
 */
case object I32 extends IntegerType[Int](3) {

  override def r(in : InStream) = in.i32()

  override def w(target : Int, out : OutStream) : Boolean = {
    val v = if (null == target) 0 else target
    out.i32(v)
    return 0 == v
  }

  override def toString = "i32"
}

/**
 * @author Timm Felden
 */
case object I64 extends IntegerType[Long](4) {

  override def r(in : InStream) = in.i64()

  override def w(target : Long, out : OutStream) : Boolean = {
    val v = if (null == target) 0 else target
    out.i64(v)
    return 0 == v
  }

  override def toString = "i64"
}

/**
 * @author Timm Felden
 */
case object V64 extends IntegerType[Long](5) {

  override def r(in : InStream) = in.v64()

  override def w(target : Long, out : OutStream) : Boolean = {
    val v = if (null == target) 0 else target
    out.v64(v)
    return 0 == v
  }

  override def toString = "v64"
}