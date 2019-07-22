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
 * Floating point types
 *
 * @author Timm Felden
 */
sealed abstract class FloatType[T](_typeID : Int) extends FieldType[T](_typeID);

/**
 * @author Timm Felden
 */
case object F32 extends FloatType[Float](6) {

  override def r(in : InStream) = in.f32()

  override def w(target : Float, out : OutStream) : Boolean = {
    val v = if (null == target) .0f else target
    out.f32(v)
    return .0f == v
  }

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Float]

  override def toString = "f32"
}

/**
 * @author Timm Felden
 */
case object F64 extends FloatType[Double](7) {

  override def r(in : InStream) = in.f64()

  override def w(target : Double, out : OutStream) : Boolean = {
    val v = if (null == target) .0 else target
    out.f64(v)
    return .0 == v
  }

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Double]

  override def toString = "f64"
}
