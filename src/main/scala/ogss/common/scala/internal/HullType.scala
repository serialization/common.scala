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

import java.util.IdentityHashMap

import scala.collection.mutable.ArrayBuffer

import ogss.common.scala.api.GeneralAccess
import ogss.common.jvm.streams.InStream
import ogss.common.jvm.streams.MappedInStream
import ogss.common.jvm.streams.OutStream

/**
 * This type subsumes all types whose serialization uses a hull-field.
 *
 * @author Timm Felden
 */
abstract class HullType[T <: AnyRef](_typeID : Int) extends ByRefType[T](_typeID) with GeneralAccess[T] {

  /**
   * The field ID used by this hull on write.
   */
  var fieldID = 0

  /**
   * The number of other fields currently depending on this type. It is set by Writer on serialization in Tco.
   *
   * @note If another field reduces deps to 0 it has to start a write job for this type.
   * @note This is in essence reference counting on an acyclic graph while writing data to disk.
   */
  var deps = 0;

  /**
   * The maximal, i.e. static, number of serialized fields depending on this type.
   *
   * @note Can be 0.
   * @note If 0, the HullType is excluded from serialization.
   */
  var maxDeps = 0;

  /**
   * get object by ID
   */
  protected[internal] val idMap = new ArrayBuffer[T]
  idMap += null.asInstanceOf[T]

  protected[internal] val IDs = new IdentityHashMap[T, Integer]

  final def resetSerialization {
    IDs.clear()

    // throw away id map, as it is no longer valid
    idMap.clear();
    idMap += null.asInstanceOf[T]
  }

  /**
   * Return the id of the argument ref. This method is thread-safe. The id returned by this function does not change
   * per invocation.
   */
  private[internal] def id(ref : T) : Int = {
    if (null == ref)
      return 0;
    this.synchronized {
      val rval = IDs.get(ref);
      if (null == rval) {
        val ID = idMap.size
        idMap += ref
        IDs.put(ref, ID);
        return ID;
      }
      return rval;
    }
  }

  final override def r(in : InStream) : T = get(in.v32())

  final override def w(v : T, out : OutStream) : Boolean = {
    if (null == v) {
      out.i8(0.toByte);
      return true;
    }

    out.v64(id(v));
    return false;
  }

  /**
   * Allocate instances when reading a file.
   *
   * @note map is positioned after count, i.e. the bucketID is still in the stream
   * @return the bucketID belonging to this allocation
   */
  protected[internal] def allocateInstances(count : Int, map : MappedInStream) : Int

  final override def size : Int = idMap.size - 1
}
