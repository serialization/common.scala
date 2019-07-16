package ogss.common.scala.internal

import scala.collection.mutable.ArrayBuffer
import java.util.IdentityHashMap
import ogss.common.streams.MappedInStream
import ogss.common.streams.InStream
import ogss.common.streams.OutStream
import ogss.common.streams.BufferedOutStream
import ogss.common.scala.api.GeneralAccess

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
   * The current number of pending blocks. 0 if the HD is not split into blocks. This number is only meaningful while
   * writing a file.
   */
  var blocks = 0

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
   * Read the hull data from the stream. Abstract, because the inner loop is type-dependent anyway.
   *
   * @note the fieldID is written by the caller
   * @return true iff hull shall be discarded (i.e. it is empty)
   */
  protected[internal] def read(block : Int, map : MappedInStream) : Unit

  /**
   * Write the hull into the stream. Abstract, because the inner loop is type-dependent anyway.
   *
   * @note the fieldID is written by the caller
   * @return true iff hull shall be discarded (i.e. it is empty)
   */
  protected[internal] def write(block : Int, out : BufferedOutStream) : Boolean

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
  
  override def toString = name;
}