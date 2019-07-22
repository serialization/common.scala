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

import ogss.common.streams.InStream
import ogss.common.streams.OutStream
import scala.collection.mutable.ArrayBuffer

/**
 * Handles enum proxies and allows enum and serialized values to be translated
 * to proxies vice versa.
 *
 * @note This is a field type AnyRef, because it is apparently not legal to
 * implement w for /\ { T#Value, EnumProxy[T] } although that would make
 * perfect sense ;)
 *
 * @author Timm Felden
 */
final class EnumPool[T <: Enumeration] private (
  _tid :     Int,
  val name : String,

  /**
 * values as seen from the combined specification
 */
  private[internal] val values : Array[EnumProxy[T]],

  /**
 * values from the perspective of the files specification, i.e. this table is used to decode values from disc
 */
  private[internal] val fileValues : Array[EnumProxy[T]],

  /**
 * values from the perspective of the tools specification, i.e. this table is used to convert enum values to proxies
 */
  private val staticValues : Array[EnumProxy[T]]

) extends FieldType[AnyRef](_tid) with Iterable[EnumProxy[T]] {

  // set owner of our proxies
  for (p ← values)
    p._owner = this

  def proxy(target : T#Value) : EnumProxy[T] = staticValues(target.id)

  override def iterator = values.iterator

  override def r(in : InStream) = fileValues(in.v32()) : AnyRef

  override def w(data : AnyRef, out : OutStream) : Boolean = {
    if (null == data) {
      out.i8(0.toByte);
      return true;
    }

    val id = data match {
      case v : T#Value      ⇒ staticValues(v.id).ID
      case v : EnumProxy[T] ⇒ v.ID
    }
    out.v64(id);
    return 0 == id;
  }

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[T#Value]

  override def toString = name
}

object EnumPool {
  /**
   * @note In Scala, enum pools cannot be constructed with a new, because constructors
   * are syntactically broken.
   */
  def apply[T <: Enumeration](
    tid :    Int,
    name :   String,
    values : Array[String],
    known :  T
  ) : EnumPool[T] = {

    if (null == values) {
      // only known values, none from file
      // @note we set file values anyway to get sane default values
      val staticValues = new Array[EnumProxy[T]](known.maxId)
      for (ki ← 0 until known.maxId) {
        staticValues(ki) = new EnumProxy[T](known(ki), known(ki).toString, ki)
      }
      new EnumPool[T](tid, name, staticValues, staticValues, staticValues)
    } else {
      val fileValues = new Array[EnumProxy[T]](values.length)

      // check if there is a known enum associated with this pool
      if (null == known) {
        for (i ← 0 until values.length) {
          fileValues(i) = new EnumProxy[T](null, values(i), i);
        }
        new EnumPool[T](tid, name, fileValues, fileValues, null)
      } else {
        val staticValues = new Array[EnumProxy[T]](known.maxId)

        // merge file values and statically known values
        val vs = new ArrayBuffer[EnumProxy[T]]

        var id = 0
        var vi = 0
        var ki = 0
        var p : EnumProxy[T] = null
        while (vi < values.length | ki < known.maxId) {
          val cmp = if (ki < known.maxId) {
            if (vi < values.length) {
              Parser.compare(values(vi), known(ki).toString)
            } else 1
          } else -1

          if (0 == cmp) {
            p = new EnumProxy[T](known(ki), values(vi), id);
            vs += p
            id += 1
            fileValues(vi) = p
            vi += 1
            staticValues(ki) = p
            ki += 1

          } else if (cmp < 0) {
            p = new EnumProxy[T](null, values(vi), id);
            vs += p
            id += 1
            fileValues(vi) = p
            vi += 1

          } else {
            p = new EnumProxy[T](known(ki), known(ki).toString, id);

            vs += p
            id += 1
            staticValues(ki) = p
            ki += 1
          }
        }

        // create values
        new EnumPool[T](tid, name,
          if (staticValues.length == fileValues.length) {
            fileValues
          } else {
            // there are unknown values
            vs.to
          }, fileValues, staticValues)
      }
    }
  }
}
