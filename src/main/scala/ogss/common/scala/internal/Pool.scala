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

import ogss.common.scala.api.Access
import ogss.common.scala.api.OGSSException
import ogss.common.scala.internal.fields.AutoField
import ogss.common.streams.InStream
import ogss.common.streams.OutStream

/**
 * Top level implementation of all storage pools.
 *
 * @param <T>
 *            static type of instances
 * @note Pools are created by a StateInitializer, only
 *
 * @author Timm Felden
 */
abstract class Pool[T <: Obj](
  _poolIndex :          Int,
  override val name :   String,
  final val superPool : Pool[_ >: T <: Obj],
  _afSize :             Int
) extends ByRefType[T](10 + _poolIndex) with Access[T] {
  private[internal] var _owner : State = _
  final override def owner = _owner

  // type hierarchy
  final override def superType : Access[_ >: T <: Obj] = superPool
  final val basePool : Pool[_ >: T <: Obj] = if (null == superPool) this else superPool.basePool
  /**
   * the number of super pools aka the height of the type hierarchy
   */
  val THH : Int = if (null == superPool) 0 else (1 + superPool.THH)

  /**
   * the next pool; used for efficient type hierarchy traversal.
   *
   * @note on setting nextPool, the bpo of nextPool will be adjusted iff it is 0 to allow insertion of types from the
   *       tool specification
   */
  private[internal] var next : Pool[_ <: Obj] = _

  /**
   * pointer to base-pool-managed data array
   */
  protected[internal] var data : Array[T] = _

  /**
   * names of known fields, the actual field information is given in the generated addKnownFiled method.
   */
  protected[internal] def KFN(ID : Int) : String = null

  /**
   * construct the known field with the given id
   */
  protected[internal] def KFC(ID : Int, SIFA : Array[FieldType[_]], nextFID : Int) : Field[_, T] = null

  /**
   * all fields that are declared as auto, including ObjectID
   *
   * @note stores fields at index "-f.index"
   * @note sub-constructor adds auto fields from super types to this array; this is an optimization to make iteration
   *       O(1); the array cannot change anyway
   * @note the initial type constructor will already allocate an array of the correct size, because the right size is
   *       statically known (a generation time constant)
   */
  protected[internal] val autoFields : Array[AutoField[_, T]] =
    if (0 == _afSize) Pool.noAutoFields.asInstanceOf[Array[AutoField[_, T]]]
    else new Array[AutoField[_, T]](_afSize)

  /**
   * all fields that hold actual data
   *
   * @note stores fields at index "f.index-1"
   */
  protected[internal] val dataFields = new ArrayBuffer[Field[_, T]]

  final override def fields = new StaticFieldIterator(this)
  final override def allFields = new FieldIterator(this)

  /**
   * The BPO of this pool relative to data.
   */
  protected[internal] var bpo : Int = _

  /**
   * All stored objects, which have exactly the type T. Objects are stored as arrays of field entries. The types of
   * the respective fields can be retrieved using the fieldTypes map.
   */
  private[internal] val newObjects = new ArrayBuffer[T]

  /**
   * Ensures that at least capacity many new objects can be stored in this pool without moving references.
   */
  final def hintNewObjectsSize(capacity : Int) {
    newObjects.sizeHint(capacity)
  }

  /**
   * Number of static instances of T in data. Managed by read/compress.
   */
  protected[internal] var staticDataInstances : Int = _

  /**
   * the number of instances of exactly this type, excluding sub-types
   *
   * @return size excluding subtypes
   */
  final def staticSize : Int = staticDataInstances + newObjects.size

  final def staticInstances = new StaticDataIterator(this)

  /**
   * size that is only valid in fixed state
   *
   * @note in contrast to SKilL/Java, we maintain this as an internal invariant only!
   */
  private[internal] var cachedSize : Int = _

  /**
   * number of deleted objects in this state
   */
  private[internal] var deletedCount : Int = 0

  protected[internal] def allocateInstances : Unit

  /**
   * Return an object by ID. Can only be used for objects with positive IDs.
   *
   * @note do not use this method if your understanding of Object IDs is not solid.
   * @note We do not allow getting objects with negative IDs because negative IDs are monomorphic. The code required
   *       to make them polymorphic is a straight forward access to owner.pool and to make a get there, but since get
   *       is used a lot, we do not want to increase its size for as little benefit as it would be to the user. Also,
   *       that solution would require a second argument of either type class or string.
   * @throws nothrow
   * @return the instance matching argument object id or null
   */
  final override def get(ID : Int) : T = {
    val index = ID - 1;

    if (null == data || (index < 0 | data.length <= index)) null.asInstanceOf[T]
    else data(index)
  }

  final override def r(in : InStream) : T = {
    val index = in.v32() - 1;

    if (index < 0 | data.length <= index) null.asInstanceOf[T]
    else data(index)
  }

  final override def w(ref : T, out : OutStream) : Boolean = {
    if (null == ref) {
      out.i8(0.toByte);
      true
    } else {
      out.v64(ref.ID);
      false
    }
  }

  /**
   * @return size including subtypes
   */
  final override def size : Int = {
    var size = 0
    val ts = new TypeHierarchyIterator(this)
    while (ts.hasNext)
      size += ts.next.staticSize
    return size;
  }

  /**
   * Add an existing instance as a new object
   *
   * @note Do not use objects managed by other OGFiles.
   */
  final def add(e : T) {
    if (e.ID != 0)
      throw new OGSSException(
        "the argument element already belongs to a state; you can transfer it by deleting in there first"
      )

    e._ID = -1 - newObjects.size
    newObjects += e
  }

  /**
   * Delete shall only be called from OGSS state
   *
   * @param target
   *            the object to be deleted
   * @note we type target using the erasure directly, because the Java type system is too weak to express correct
   *       typing, when taking the pool from a map
   */
  private[internal] final def delete(target : Obj) {
    val ID = target._ID;
    if (0 != ID) {
      // check that target is in fact managed by this state
      if ((0 < ID & null != data && ID <= data.length && target == data(ID - 1))
        || (ID < 0 & -ID <= newObjects.size && target == newObjects(-1 - ID))) {
        target._ID = 0
        deletedCount += 1
      } else {
        throw new OGSSException("cannot delete an object that is not managed by this pool");
      }
    }
  }

  final override def iterator = new DynamicDataIterator(this)
  final override def inTypeOrder = new TypeOrderIterator(this)

  /**
   * Get the name of known sub pool with argument local id. Return null, if id is invalid.
   */
  protected[internal] def nameSub(ID : Int) : String = null

  /**
   * Create the known sub pool with argument local id. Return null, if id is invalid.
   */
  protected[internal] def makeSub(ID : Int, index : Int) : Pool[_ >: T <: Obj] = null

  /**
   * Create an unknown sub pool with the argument name
   */
  protected[internal] def makeSub(index : Int, name : String) : Pool[_ >: T <: Obj] =
    new SubPool[UnknownObject](index, name, classOf[UnknownObject],
      this.asInstanceOf[Pool[Obj]]).asInstanceOf[Pool[_ >: T <: Obj]]
}

object Pool {

  /**
   * used as placeholder, if there are no auto fields at all to optimize allocation time and memory usage
   */
  private val noAutoFields = new Array[AutoField[_, _]](0)
}
