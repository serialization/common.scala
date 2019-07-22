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

import ogss.common.scala.api.FieldAccess
import ogss.common.scala.internal.fields.AutoField
import scala.collection.mutable.HashSet
import ogss.common.streams.MappedInStream
import ogss.common.streams.BufferedOutStream
import ogss.common.scala.internal.restrictions.FieldRestriction
import scala.collection.mutable.HashMap
import scala.collection.mutable.Iterable

/**
 * Actual implementation as used by all bindings.
 *
 * @author Timm Felden
 */
abstract class Field[T, Ref <: Obj](
  override val t :     FieldType[T],
  override val name :  String,
  val ID :             Int,
  override val owner : Pool[Ref]
) extends FieldAccess[T] {
  assert(null != t)

  // register field
  if (ID < 0)
    // auto fields get per-type negative IDs
    owner.autoFields(-1 - ID) = this.asInstanceOf[AutoField[T, Ref]]
  else
    owner.dataFields += this

  /**
   * The current number of pending blocks. 0 if FD is not split into blocks. This number is only meaningful while
   * writing a file.
   */
  var blocks = 0

  /**
   * Restriction handling.
   */
  val restrictions = new HashSet[FieldRestriction[T]]

  /**
   * Check consistency of restrictions on this field. Also, check that stored
   * values belong to the same state and that these values are type-correct.
   */
  private[internal] final def check(x : Ref) {
    this match {
      case self : LazyField[T, Ref] ⇒ self.ensureLoaded
      case _                        ⇒
    }

    if (!x.isDeleted) {
      // check restrictions
      for (r ← restrictions) r.check(get(x));

      // ensure that values are contained in this state iff they could be serialized
      if (!this.isInstanceOf[AutoField[T, Ref]])
        Field.contained(get(x), owner.owner, s"${x.getClass.getName}#${x._ID}.$name")

      // type check the value
      if (!t.typeCheck(get(x))) {
        owner.owner.checkErrors.add(s"${x.getClass.getName}#${x._ID}.$name stores a value which is not well-typed")
      }
    }
  }

  /**
   * Read data from a mapped input stream and set it accordingly. This is invoked at the very end of state
   * construction and done massively in parallel.
   */
  def read(i : Int, h : Int, in : MappedInStream)

  /**
   * write data into a map at the end of a write/append operation
   *
   * @note only called, if there actually is field data to be written
   * @return true iff the written data contains default values only
   */
  def write(i : Int, h : Int, out : BufferedOutStream) : Boolean

  override def toString = t.toString + " " + name
}

object Field {
  def contained(v : Any, in : State, from : String) : Unit = v match {
    case null    ⇒ // ok
    case v : Obj ⇒ if (!in.contains(v)) in.checkErrors.add(s"$from refers extern Object ${v.getClass.getName}#${v._ID}")
    case v : HashMap[_, _] ⇒
      v.keySet.foreach(contained(_, in, from))
      v.values.foreach(contained(_, in, from))
    case v : Iterable[_] ⇒ v.foreach(contained(_, in, from))
    case _               ⇒ // not an issue
  }
}