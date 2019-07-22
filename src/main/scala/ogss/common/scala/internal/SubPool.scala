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

/**
 * A generic sub pool class that creates new objects via reflection to reduce the amount of generated code.
 *
 * @author Timm Felden
 */
final class SubPool[T <: Obj](
  _poolIndex :      Int,
  _name :           String,
  private val cls : Class[T],
  _super :          Pool[_ >: T <: Obj]
) extends Pool[T](_poolIndex, _name, _super, 0) {

  override def typeCheck(x : Any) : Boolean = null == x || cls.isAssignableFrom(x.getClass())

  protected[internal] override def makeSub(index : Int, name : String) = new SubPool(index, name, cls, this)

  protected[internal] override def allocateInstances {
    var i = bpo
    var j = 0
    val high = i + staticDataInstances

    val make = cls.getConstructor(classOf[Pool[_]], classOf[Int]);
    while (i < high) {
      j = (i + 1)
      data(i) = make.newInstance(this, Int.box(j))
      i = j;
    }
  }

  override def make = {
    val r = cls.getConstructor(classOf[Pool[_]], classOf[Int]).newInstance(this, Int.box(-1 - newObjects.size))
    newObjects += r
    r
  }
}
