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
 * Iterates efficiently over static instances of a pool.
 *
 * @author Timm Felden
 */
final class StaticDataIterator[T <: Obj](val p : Pool[T]) extends Iterator[T] {
  var index = p.bpo
  var last = index + p.staticDataInstances

  var second = false

  // find first valid position
  if (index == last) {
    second = true
    index = 0
    last = p.newObjects.size
  }

  override def hasNext = index < last

  override def next : T =
    if (!second) {
      val r = p.data(index)
      index += 1
      if (index == last) {
        second = true;
        index = 0;
        last = p.newObjects.size
      }
      r.asInstanceOf[T]
    } else {

      val r = p.newObjects(index);
      index += 1
      r
    }
}
