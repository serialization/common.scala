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

import scala.collection.mutable.ArrayBuffer
import ogss.common.streams.InStream
import ogss.common.streams.OutStream

/**
 * AnyRef types are instantiated once per state.
 *
 * @param types
 *            the array list containing all types valid inside of a state
 * @note types can grow after passing the pointer to the annotation type. This behavior is required in order to
 *       implement reflective annotation parsing correctly.
 * @note can not take a state as argument, because it may not exist yet
 *
 * @author Timm Felden
 */
final class AnyRefType(

  private val types : ArrayBuffer[Pool[_ <: Obj]]

) extends ByRefType[Obj](8) {

  private[internal] var _owner : State = _
  def owner : State = _owner

  override def r(in : InStream) : Obj = {
    val t = in.v32();
    if (0 == t) {
      null
    } else {
      val f = in.v32();
      types(t - 1).get(f)
    }
  }

  override def w(ref : Obj, out : OutStream) : Boolean = {
    if (null == ref) {
      // magic trick!
      out.i8(0.toByte);

      true
    } else {

      val stid = ref.STID
      val p =
        if (-1 != stid) _owner.SIFA(stid).asInstanceOf[Pool[_]]
        else ref.asInstanceOf[NamedObj].τp

      out.v64(p.typeID - 9);
      out.v64(ref._ID);

      false
    }
  }

  override def toString = "anyRef"
}
