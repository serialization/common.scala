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

import ogss.common.streams.MappedInStream

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

  // is loaded <-> input == null
  var input : MappedInStream = null

  private final def load {
    // we recycled first and last ID, so it is already set as intended
    val high = lastID - firstID;
    var i = 0;
    data = new Array[Any](high)
    while (i != high) {
      data(i) = t.r(input)
      i += 1
    }

    if (!input.eof())
      throw new IOException("lazy read task did not consume InStream");

    input = null
  }

  // required to ensure that data is present before state reorganization
  final def ensureLoaded {
    if (null != input)
      load
  }

  final override def get(ref : Obj) : T = {
    val ID = ref._ID - 1
    if (ID < 0)
      return newData.get(ref).asInstanceOf[T]

    if (null != input)
      load

    if (ID >= lastID)
      throw new IndexOutOfBoundsException("illegal access to distributed field");

    return data(ID - firstID).asInstanceOf[T]
  }

  final override def set(ref : Obj, value : T) {
    val ID = ref._ID - 1
    if (ID < 0)
      newData.put(ref.asInstanceOf[Ref], value)

    if (null != input)
      load

    if (ID >= lastID)
      throw new IndexOutOfBoundsException("illegal access to distributed field");

    data(ID - firstID) = value
  }

  final override def read(i : Int, h : Int, in : MappedInStream) {
    firstID = i;
    lastID = h;
    input = in;
  }
}
