/*******************************************************************************
 * Copyright 2019 University of Stuttgart
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License.  You may obtain a copy
 * of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
 * License for the specific language governing permissions and limitations under
 * the License.
 ******************************************************************************/
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

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Byte]

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

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Short]

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

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Int]

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

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Long]

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

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Long]

  override def toString = "v64"
}
