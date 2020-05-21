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

import ogss.common.scala.internal.HullType
import ogss.common.scala.internal.State
import ogss.common.scala.internal.FieldType
import scala.collection.mutable.ArrayBuffer
import ogss.common.scala.Constants
import ogss.common.jvm.streams.MappedInStream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import ogss.common.jvm.streams.BufferedOutStream

/**
 * Super class of all container types
 *
 * @author Timm Felden
 */
sealed abstract class ContainerType[T <: AnyRef](_typeID : Int) extends HullType[T](_typeID) {

  /**
   * The current number of pending blocks. 0 if the HD is not split into blocks. This number is only meaningful while
   * writing a file.
   */
  var blocks = 0

  /**
   * Read the hull data from the stream. Abstract, because the inner loop is type-dependent anyway.
   *
   * @note the fieldID is written by the caller
   * @return true iff hull shall be discarded (i.e. it is empty)
   */
  protected[internal] def read(i : Int, end : Int, map : MappedInStream) : Unit

  /**
   * Write the hull into the stream. Abstract, because the inner loop is type-dependent anyway.
   *
   * @note the fieldID is written by the caller
   * @return true iff hull shall be discarded (i.e. it is empty)
   */
  protected[internal] def write(i : Int, end : Int, out : BufferedOutStream) : Unit

  final override def get(ID : Int) : T = idMap(ID)

  final override def iterator = {
    val r = idMap.iterator
    r.next() // skip null
    r
  }

  final override def owner : State = throw new Error("TODO")
}

/**
 * Super class of all container types with one type argument
 *
 * @author Timm Felden
 */
sealed abstract class SingleArgumentType[T <: Iterable[Base], Base](
  _typeID :  Int,
  val base : FieldType[Base],
  val kind : Byte
) extends ContainerType[T](_typeID);

final class ArrayType[T](
  _typeID : Int,
  _base :   FieldType[T]
) extends SingleArgumentType[ArrayBuffer[T], T](_typeID, _base, 0) {

  override val name = base + "[]"

  override def typeCheck(x : Any) : Boolean = x match {
    case xs : ArrayBuffer[_] ⇒ xs.forall(base.typeCheck)
    case _                   ⇒ false
  }

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count > Constants.HD_Threshold) {
      val block = in.v32();

      // initialize idMap with null to allow parallel updates
      this.synchronized {
        if (1 == idMap.size) {
          var c = count;
          while (c != 0) {
            c -= 1
            idMap += null
          }
        }
      }
      var i = block * Constants.HD_Threshold;
      val end = Math.min(count, i + Constants.HD_Threshold);

      while (i < end) {
        i += 1
        idMap(i) = new ArrayBuffer
      }

      return block;
    }
    // else, no blocks
    var i = count
    while (i != 0) {
      i -= 1
      idMap += new ArrayBuffer
    }
    return 0;
  }

  override def read(begin : Int, end : Int, in : MappedInStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i)
      var s = in.v32();
      while (s != 0) {
        s -= 1
        xs += base.r(in)
      }
    }
  }

  override def write(begin : Int, end : Int, out : BufferedOutStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i);
      out.v64(xs.size);
      for (x ← xs) {
        base.w(x, out);
      }
    }
  }
}

final class ListType[T](
  _typeID : Int,
  _base :   FieldType[T]
) extends SingleArgumentType[ListBuffer[T], T](_typeID, _base, 1) {

  override val name = s"list<$base>"

  override def typeCheck(x : Any) : Boolean = x match {
    case xs : ListBuffer[_] ⇒ xs.forall(base.typeCheck)
    case _                  ⇒ false
  }

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count > Constants.HD_Threshold) {
      val block = in.v32();

      // initialize idMap with null to allow parallel updates
      this.synchronized {
        if (1 == idMap.size) {
          var c = count;
          while (c != 0) {
            c -= 1
            idMap += null
          }
        }
      }
      var i = block * Constants.HD_Threshold;
      val end = Math.min(count, i + Constants.HD_Threshold);

      while (i < end) {
        i += 1
        idMap(i) = new ListBuffer
      }

      return block;
    }
    // else, no blocks
    var i = count
    while (i != 0) {
      i -= 1
      idMap += new ListBuffer
    }
    return 0;
  }

  override def read(begin : Int, end : Int, in : MappedInStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i)
      var s = in.v32();
      while (s != 0) {
        s -= 1
        xs += base.r(in)
      }
    }
  }

  override def write(begin : Int, end : Int, out : BufferedOutStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i);
      out.v64(xs.size);
      for (x ← xs) {
        base.w(x, out);
      }
    }
  }
}

final class SetType[T](
  _typeID : Int,
  _base :   FieldType[T]
) extends SingleArgumentType[HashSet[T], T](_typeID, _base, 2) {

  override val name = s"set<$base>"

  override def typeCheck(x : Any) : Boolean = x match {
    case xs : HashSet[_] ⇒ xs.forall(base.typeCheck)
    case _               ⇒ false
  }

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count > Constants.HD_Threshold) {
      val block = in.v32();

      // initialize idMap with null to allow parallel updates
      this.synchronized {
        if (1 == idMap.size) {
          var c = count;
          while (c != 0) {
            c -= 1
            idMap += null
          }
        }
      }
      var i = block * Constants.HD_Threshold;
      val end = Math.min(count, i + Constants.HD_Threshold);

      while (i < end) {
        i += 1
        idMap(i) = new HashSet
      }

      return block;
    }
    // else, no blocks
    var i = count
    while (i != 0) {
      i -= 1
      idMap += new HashSet
    }
    return 0;
  }

  override def read(begin : Int, end : Int, in : MappedInStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i)
      var s = in.v32();
      while (s != 0) {
        s -= 1
        xs += base.r(in)
      }
    }
  }

  override def write(begin : Int, end : Int, out : BufferedOutStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i);
      out.v64(xs.size);
      for (x ← xs) {
        base.w(x, out);
      }
    }
  }
}

final class MapType[K, V](
  _typeID :       Int,
  val keyType :   FieldType[K],
  val valueType : FieldType[V]
) extends ContainerType[HashMap[K, V]](_typeID) {

  override val name = s"map<$keyType,$valueType>"

  override def typeCheck(x : Any) : Boolean = x match {
    case xs : HashMap[_, _] ⇒ xs.keys.forall(keyType.typeCheck) && xs.values.forall(valueType.typeCheck)
    case _                  ⇒ false
  }

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count > Constants.HD_Threshold) {
      val block = in.v32();

      // initialize idMap with null to allow parallel updates
      this.synchronized {
        if (1 == idMap.size) {
          var c = count;
          while (c != 0) {
            c -= 1
            idMap += null
          }
        }
      }
      var i = block * Constants.HD_Threshold;
      val end = Math.min(count, i + Constants.HD_Threshold);

      while (i < end) {
        i += 1
        idMap(i) = new HashMap
      }

      return block;
    }
    // else, no blocks
    var i = count
    while (i != 0) {
      i -= 1
      idMap += new HashMap
    }
    return 0;
  }

  override def read(begin : Int, end : Int, in : MappedInStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i)
      var s = in.v32()
      while (s != 0) {
        s -= 1
        val k = keyType.r(in)
        val v = valueType.r(in)
        xs(k) = v
      }
    }
  }

  override def write(begin : Int, end : Int, out : BufferedOutStream) {
    var i = begin
    while (i < end) {
      i += 1
      val xs = idMap(i);
      out.v64(xs.size);
      for (e ← xs) {
        keyType.w(e._1, out);
        valueType.w(e._2, out);
      }
    }
  }
}
