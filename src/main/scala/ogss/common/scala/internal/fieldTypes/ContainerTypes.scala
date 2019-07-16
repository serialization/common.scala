package ogss.common.scala.internal.fieldTypes

import ogss.common.scala.internal.HullType
import ogss.common.scala.internal.State
import ogss.common.scala.internal.FieldType
import scala.collection.mutable.ArrayBuffer
import ogss.common.scala.Constants
import ogss.common.streams.MappedInStream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import ogss.common.streams.BufferedOutStream

/**
 * Super class of all container types
 *
 * @author Timm Felden
 */
sealed abstract class ContainerType[T <: AnyRef](_typeID : Int) extends HullType[T](_typeID) {

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

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count >= Constants.HD_Threshold) {
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

  override def read(block : Int, in : MappedInStream) {
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    while ({
      i += 1
      i < end
    }) {
      val xs = idMap(i)
      var s = in.v32();
      while (s != 0) {
        s -= 1
        xs += base.r(in)
      }
    }
  }

  override def write(block : Int, out : BufferedOutStream) : Boolean = {
    val count = idMap.size - 1;
    if (0 == count) {
      return true;
    }

    out.v64(count);
    if (count >= Constants.HD_Threshold) {
      out.v64(block);
    }
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    while ({
      i += 1
      i < end
    }) {
      val xs = idMap(i);
      out.v64(xs.size);
      for (x ← xs) {
        base.w(x, out);
      }
    }
    return false;
  }
}

final class ListType[T](
  _typeID : Int,
  _base :   FieldType[T]
) extends SingleArgumentType[ListBuffer[T], T](_typeID, _base, 1) {

  override val name = s"list<$base>"

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count >= Constants.HD_Threshold) {
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

  override def read(block : Int, in : MappedInStream) {
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    while ({
      i += 1
      i < end
    }) {
      val xs = idMap(i)
      var s = in.v32();
      while (s != 0) {
        s -= 1
        xs += base.r(in)
      }
    }
  }

  override def write(block : Int, out : BufferedOutStream) : Boolean = {
    val count = idMap.size - 1;
    if (0 == count) {
      return true;
    }

    out.v64(count);
    if (count >= Constants.HD_Threshold) {
      out.v64(block);
    }
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    while ({
      i += 1
      i < end
    }) {
      val xs = idMap(i);
      out.v64(xs.size);
      for (x ← xs) {
        base.w(x, out);
      }
    }
    return false;
  }
}

final class SetType[T](
  _typeID : Int,
  _base :   FieldType[T]
) extends SingleArgumentType[HashSet[T], T](_typeID, _base, 2) {

  override val name = s"set<$base>"

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count >= Constants.HD_Threshold) {
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

  override def read(block : Int, in : MappedInStream) {
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    while ({
      i += 1
      i < end
    }) {
      val xs = idMap(i)
      var s = in.v32();
      while (s != 0) {
        s -= 1
        xs += base.r(in)
      }
    }
  }

  override def write(block : Int, out : BufferedOutStream) : Boolean = {
    val count = idMap.size - 1;
    if (0 == count) {
      return true;
    }

    out.v64(count);
    if (count >= Constants.HD_Threshold) {
      out.v64(block);
    }
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    while ({
      i += 1
      i < end
    }) {
      val xs = idMap(i);
      out.v64(xs.size);
      for (x ← xs) {
        base.w(x, out);
      }
    }
    return false;
  }
}

final class MapType[K, V](
  _typeID :       Int,
  val keyType :   FieldType[K],
  val valueType : FieldType[V]
) extends ContainerType[HashMap[K, V]](_typeID) {

  override val name = s"map<$keyType,$valueType>"

  protected[internal] final override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    // check for blocks
    if (count >= Constants.HD_Threshold) {
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

  protected[internal] final override def read(block : Int, in : MappedInStream) {
    var i = block * Constants.HD_Threshold;
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    i += 1
    while (i < end) {
      val xs = idMap(i)
      var s = in.v32()
      while (s != 0) {
        s -= 1
        val k = keyType.r(in)
        val v = valueType.r(in)
        xs(k) = v
      }
      i += 1
    }
  }

  protected[internal] final override def write(block : Int, out : BufferedOutStream) : Boolean = {
    val count = idMap.size - 1;
    if (0 == count) {
      return true;
    }

    out.v64(count);
    if (count >= Constants.HD_Threshold) {
      out.v64(block);
    }
    var i = block * Constants.HD_Threshold
    val end = Math.min(idMap.size, i + Constants.HD_Threshold);
    i += 1
    while (i < end) {
      val xs = idMap(i);
      out.v64(xs.size);
      for (e ← xs) {
        keyType.w(e._1, out);
        valueType.w(e._2, out);
      }
      i += 1
    }
    return false;
  }
}