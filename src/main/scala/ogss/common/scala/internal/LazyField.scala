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

import java.io.IOException

import ogss.common.jvm.streams.MappedInStream
import scala.collection.mutable.ArrayBuffer

/**
 * The field is distributed and loaded on demand. Unknown fields are lazy as well.
 *
 * @note implementation abuses a distributed field that can be accessed iff there are no data chunks to be processed
 *
 * @note offset and write methods will not be overwritten, because forcing has to happen even before compress
 *
 * @author Timm Felden
 */
class LazyField[T, Ref <: Obj](
  _t :     FieldType[T],
  _name :  String,
  _index : Int,
  _owner : Pool[Ref]
) extends DistributedField[T, Ref](_t, _name, _index, _owner) {
  private case class Chunk(val begin : Int, val end : Int, val in : MappedInStream)

  // is loaded <-> input == null
  private var chunks : ArrayBuffer[Chunk] = null

  private final def load {
    for (Chunk(begin, end, in) ← chunks) {
      super.read(begin, end, in)

      if (!in.eof())
        throw new IOException("lazy read task did not consume InStream");
    }

    chunks = null
  }

  // required to ensure that data is present before state reorganization
  final def ensureLoaded {
    if (null != chunks)
      load
  }

  final override def get(ref : Obj) : T = {
    val ID = ref._ID
    if (ID <= 0) {
      var r = newData.get(ref)
      if (null == r & t.typeID < 8) {
        r = FieldType.defaultValue(t).asInstanceOf[T]
        newData.put(ref.asInstanceOf[Ref], r)
      }
      return r
    }

    if (ID < firstID || ID >= lastID)
      throw new IndexOutOfBoundsException("illegal access to distributed field");

    if (null != chunks)
      load

    var r = data(ID - firstID).asInstanceOf[T]
    if (null == r & t.typeID < 8) {
      r = FieldType.defaultValue(t).asInstanceOf[T]
      data(ID - firstID) = r
    }
    return r
  }

  final override def set(ref : Obj, value : T) {
    val ID = ref._ID
    if (ID <= 0)
      newData.put(ref.asInstanceOf[Ref], value)

    if (ID < firstID || ID >= lastID)
      throw new IndexOutOfBoundsException("illegal access to distributed field");

    if (null != chunks)
      load

    data(ID - firstID) = value
  }

  final override def read(i : Int, h : Int, in : MappedInStream) {
    synchronized {
      if (null == chunks) {
        chunks = new ArrayBuffer
      }
      chunks += Chunk(i, h, in)
    }
  }
}
