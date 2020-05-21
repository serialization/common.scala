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
package ogss.common.scala.internal

import ogss.common.jvm.streams.{InStream, OutStream}

/**
 * Top level implementation of a field type, the runtime representation of a fields type.
 *
 * @param T type of field values
 * @note representation of the type system relies on invariants and heavy abuse of type erasure
 * @author Timm Felden
 */
abstract class FieldType[T](_typeID : Int) extends ogss.common.scala.api.FieldType[T](_typeID) {

  /**
   * Read one T out of the stream.
   *
   * @note this function has to be implemented by FieldTypes because of limits of the Java type system (and any other
   *       sane type system)
   * @note intended for internal usage only!
   */
  def r(in : InStream): T

  /**
   * Write one T into the stream.
   *
   * @note this function has to be implemented by FieldTypes because of limits of the Java type system (and any other
   *       sane type system)
   * @note intended for internal usage only!
   * @return true iff a default value was written
   */
  def w(data : T, out : OutStream) : Boolean

  /**
   * Check if a value is actually of the required type.
   * This function is necessary to overcome restrictions of type erasure.
   */
  def typeCheck(v : Any) : Boolean
}

object FieldType {
  /**
   * Create a nonnull boxed default value to work around autounboxing NPE
   * madness
   */
  final def defaultValue(t : FieldType[_]) : Any = t.typeID match {
    case 0 ⇒ false
    case 1 ⇒ 0.toByte
    case 2 ⇒ 0.toShort
    case 3 ⇒ 0.toInt
    case 4 ⇒ 0.toLong
    case 5 ⇒ 0.toLong
    case 6 ⇒ 0.toFloat
    case 7 ⇒ 0.toDouble
    case _ ⇒ null
  }
}
