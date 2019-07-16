package ogss.common.scala.internal

import java.util.IdentityHashMap

import ogss.common.scala.internal.fieldTypes.Bool
import ogss.common.streams.BoolOutWrapper
import ogss.common.streams.BufferedOutStream
import ogss.common.streams.MappedInStream

/**
 * The fields data is distributed into an array (or hash map for new objects) holding its instances.
 *
 * @note implements OGSS/C++ strategy
 *
 * @author Timm Felden
 */
class DistributedField[T, Ref <: Obj](
  _t :     FieldType[T],
  _name :  String,
  _index : Int,
  _owner : Pool[Ref]
) extends Field[T, Ref](_t, _name, _index, _owner) {

  /**
   * data is shifted by owner.bpo + 1
   *
   * @note valid iff data != null (lazy field will reuse this beforeallocation of data)
   */
  private[internal] var firstID = 0

  /**
   * data holds pointers in [firstID; lastID[
   *
   * @note valid iff data != null (lazy field will reuse this before allocation of data)
   */
  private[internal] var lastID = 0

  /**
   * field data corresponding to Pool.data
   * @note the array contains data for 0 -> (lastID-firstID), i.e. access has
   * to be shifted by firstID
   */
  private[internal] var data : Array[Any] = null

  /**
   * field data corresponding to newObjects in Pool and its sub pools
   */
  private[internal] val newData = new IdentityHashMap[Ref, T]

  override def get(ref : Obj) : T = {
    val ID = ref._ID - 1
    if (ID < 0)
      return newData.get(ref)

    if (ID >= lastID)
      throw new IndexOutOfBoundsException("illegal access to distributed field");

    return data(ID - firstID).asInstanceOf[T]
  }

  override def set(ref : Obj, value : T) {
    val ID = ref._ID - 1
    if (ID < 0)
      newData.put(ref.asInstanceOf[Ref], value)

    if (ID >= lastID)
      throw new IndexOutOfBoundsException("illegal access to distributed field");

    data(ID - firstID) = value
  }

  override def read(begin : Int, end : Int, in : MappedInStream) {
    // we fill in data and data is nullptr at this point, so we have to allocate it first
    firstID = begin;
    lastID = end;
    val high = end - begin;
    var i = 0;
    data = new Array[Any](high)
    while (i != high) {
      data(i) = t.r(in)
      i += 1
    }
  }

  final def compress(newLBPO : Int) {
    // create new data
    val d = new Array[Any](_owner.cachedSize)

    // calculate new data
    // note: data could be null
    var next = 0;
    if (null != data) {
      val is = _owner.iterator
      while (is.hasNext) {
        val i = is.next
        val ID = i._ID
        if (0 != ID) {
          d(next) =
            if (ID < 0) newData.get(i)
            else data((ID - 1) - firstID)
          next += 1
        }
      }
    } else {
      val is = _owner.iterator
      while (is.hasNext) {
        val i = is.next
        val ID = i._ID
        if (0 != ID) {
          d(next) =
            if (ID < 0) newData.get(i)
            else null
          next += 1
        }
      }
    }

    // update state
    data = d
    firstID = newLBPO
    lastID = firstID + _owner.cachedSize
    assert(next == _owner.cachedSize)

    newData.clear();
  }

  override def write(first : Int, h : Int, out : BufferedOutStream) : Boolean = {
    var drop = true;
    (t : FieldType[_]) match {
      case Bool ⇒ {
        val wrap = new BoolOutWrapper(out);
        var i = first
        while (i < h) {
          val v = java.lang.Boolean.TRUE == data(i - firstID)
          wrap.bool(v);
          drop &= !v;
          i += 1
        }
        wrap.unwrap();
      }
      case _ ⇒ {
        // it is always data and therefore shifted
        val high = h - first;
        var i = 0;
        while (i < high) {
          drop &= t.w(data(i).asInstanceOf[T], out)
          i += 1
        }
      }
    }
    return drop;
  }
}