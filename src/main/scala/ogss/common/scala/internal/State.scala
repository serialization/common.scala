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

import scala.collection.mutable.HashMap
import ogss.common.scala.api.Access
import java.nio.file.Path
import java.io.IOException
import ogss.common.streams.FileOutputStream
import ogss.common.scala.api.OGSSException
import ogss.common.scala.api.GeneralAccess
import ogss.common.scala.internal.fieldTypes.ContainerType
import ogss.common.scala.api.Mode
import ogss.common.scala.api.Write

abstract class State(_init : StateInitializer) extends AutoCloseable {

  /**
   * the guard of the file must not contain \0-characters.
   */
  var guard : String

  // types by OGSS name
  private var TBN : HashMap[String, FieldType[_]] = null

  /**
   * @return pool for a given type name
   */
  final def pool(name : String) : Pool[_] = {
    if (null == TBN) {
      TBN = new HashMap
      for (p ← classes) {
        TBN(p.name) = p
      }
    }
    return TBN(name).asInstanceOf[Pool[_]];
  }

  /**
   * @return the pool corresponding to the dynamic type of the argument Obj
   * @note you might get a corresponding pool even if ref is not owned by it
   * @note inv: result.owner == this || result == null
   */
  final def pool(ref : Obj) : Pool[_] = {
    if (null == ref) {
      return null;
    } else if (ref.isInstanceOf[NamedObj]) {
      val r = ref.asInstanceOf[NamedObj].τp
      if (r.owner == this)
        return r;
    } else {
      val TID = ref.STID;
      if (TID < SIFA.length)
        return SIFA(TID).asInstanceOf[Pool[_]];
    }
    return null;
  }

  /**
   * a file input stream keeping the handle to a file for potential write operations
   *
   * @note this is a consequence of the retarded windows file system
   */
  private var input = _init match {
    case init : Parser ⇒ init.in
    case _             ⇒ null
  }

  private[internal] val strings = _init.strings
  strings._owner = this;
  /**
   * @return access to known strings
   */
  final def Strings : GeneralAccess[String] = strings

  // field types by statically known ID
  // note to self: try to represent this as 0-size array in C++ to bypass unions or other hacks
  protected[internal] val SIFA : Array[FieldType[_]]

  /**
   * @return the type name of the type of an object.
   */
  final def typeName(ref : Obj) : String = pool(ref).name;

  // types in type order
  protected[internal] val classes : Array[Pool[_ <: Obj]] = _init.classes.to

  for (p ← classes) {
    p._owner = this;
  }

  protected[internal] val containers : Array[ContainerType[_]] = _init.containers.to
  protected[internal] val enums : Array[EnumPool[_ <: Enumeration]] = _init.enums.to

  /**
   * @return iterator over all user types
   */
  final def allTypes : Iterable[_ <: Access[_ <: Obj]] = classes.asInstanceOf[Iterable[_ <: Access[_ <: Obj]]]

  /**
   * Types required for reflective IO
   */
  protected[internal] val anyRefType : AnyRefType = _init.anyRef
  anyRefType._owner = this;

  /**
   * @return true, iff the argument object is managed by this state and not marked deleted
   * @note will return false if argument is null or deleted; an iterator can still return the argument deleted ref
   *       unless a flush happened after the delete
   * @note this operation is kind of expensive
   */
  final def contains(ref : Obj) : Boolean = {
    if (null == ref)
      return false;

    val ID = ref._ID
    if (0 == ID)
      return false;

    try {
      val p = pool(ref);

      if (0 < ID)
        return ref == p.data(ID - 1)

      return ref == p.newObjects(-1 - ID)
    } catch {
      // out of bounds or similar mean its not one of ours
      case _ ⇒ return false;
    }
  }

  /**
   * ensure that the argument instance will be deleted on next flush
   *
   * @note safe behaviour for null and duplicate delete
   */
  final def delete(ref : Obj) {
    if (null != ref && ref._ID != 0) {
      pool(ref).delete(ref);
    }
  }

  /**
   * True iff the state can perform write operations.
   */
  private var canWrite = _init.canWrite;

  /**
   * Set a new mode. The only useful application is to set mode to ReadOnly.
   */
  final def changeMode(writeMode : Mode) {
    // check illegal change
    if (!canWrite)
      throw new IllegalArgumentException("Cannot change from read only, to a write mode.");

    this.canWrite = Write == writeMode;
  }

  /**
   * @return the current path pointing to the file
   */
  final def currentPath : Path = path

  /**
   * path that will be targeted as binary file
   */
  private var path = _init.path;

  /**
   * Set a new output path for the file. This will influence the next flush/close operation.
   *
   * @note The mode will be set to Write.
   * @note (on implementation) memory maps for lazy evaluation must have been created before invocation of this method
   */
  final def changePath(path : Path) {
    this.canWrite = true;
    this.path = path;
  }

  /**
   * Force all lazy string and field data to be loaded from disk.
   */
  final def loadLazyData() {
    // check if the file input stream is still open
    if (null == input)
      return ;

    // ensure that lazy fields have been loaded
    for (p ← classes; f ← p.dataFields)
      f match {
        case f : LazyField[_, _] ⇒ f.ensureLoaded
        case _                   ⇒
      }

    // all strings have been loaded by now
    strings.dropRB

    // close the file input stream and ensure that it is not read again
    try {
      input.close();
    } catch {
      case e : IOException ⇒ throw new RuntimeException("internal error", e);
    }
    input = null;
  }

  /**
   * Checks consistency of the current state of the file.
   *
   * @note it is possible to fix the inconsistency and re-check without breaking the on-disk representation
   * @throws OGSSException
   *             if an inconsistency is found
   */
  final def check {
    ???
    // TODO type checks!
    // TODO type restrictions
    // TODO make pools check fields, because they can optimize checks per

    // TODO check that idMaps do not contain unintended null-entries

    // instance and remove redispatching, if no
    // restrictions apply anyway
    for (p ← classes; f ← p.dataFields)
      try {
        f.check();
        // TODO check sollte eine fehlerliste erzeugen anstatt dieser ogss exceptions und dann eine geordnete exception produzieren
      } catch {
        case e : OGSSException ⇒
          throw new OGSSException(
            String.format("check failed in %s.%s:\n  %s", p.name, f.name, e.getMessage()), e
          );
      }
  }

  /**
   * Calculate a closure, i.e. add all reachable Objs to this state.
   *
   * @todo implement!
   * @throws OGSSException
   */
  final def closure() {
    ???
  }

  /**
   * Drops types and fields from this state that are currently unused.
   *
   * @note it is an error to modify the state after calling this function
   */
  final def dropUnusedTypes() {
    ???
  }

  /**
   * Check consistency and write changes to disk.
   *
   * @note this will not sync the file to disk, but it will block until all in-memory changes are written to buffers.
   * @throws OGSSException
   *             if check fails
   */
  final def flush {
    if (!canWrite)
      throw new OGSSException("Cannot flush a read only file. Note: close will turn a file into read only.");
    try {
      loadLazyData();
      new Writer(this, new FileOutputStream(path));
      return ;
    } catch {
      case e : OGSSException ⇒ throw e
      case e : IOException   ⇒ throw new OGSSException("failed to create or complete out stream", e)
      case e : Exception     ⇒ throw new OGSSException("unexpected exception", e);
    }
  }

  /**
   * Same as flush, but will also sync and close file, thus the state must not be used afterwards.
   */
  override final def close {
    // flush if required
    if (canWrite) {
      flush
      canWrite = false
    }

    // close file stream to work around issue with broken Windows FS
    if (null != input) {
      try {
        input.close();
      } catch {
        case e : IOException ⇒
          // we don't care
          e.printStackTrace();
      }
      input = null;
    }
  }
}
