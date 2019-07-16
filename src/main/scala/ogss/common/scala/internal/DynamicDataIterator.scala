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
 * Iterates efficiently over dynamic instances of a pool. First phase will iterate over all blocks of the pool. The
 * second phase will iterate over all dynamic instances of the pool.
 *
 * @author Timm Felden
 */
final class DynamicDataIterator[T <: Obj](private var p : Pool[_ <: T]) extends Iterator[T] {
  val end = p.THH;
  var index = p.bpo
  var last = index + p.cachedSize
  var second = false

  // find an instance in first phase
  if (index == last) {
    second = true;
    while (null != p && 0 == p.newObjects.size) {
      val n = p.next.asInstanceOf[Pool[_ <: T]]
      p = if (null != n && end < n.THH) n else null
    }
    if (null != p) {
      index = 0;
      last = p.newObjects.size
    }
  }

  override def hasNext = null != p

  override def next = if (!second) {
    val r = p.data(index).asInstanceOf[T]
    index += 1
    // mode switch, as we reached the end of data
    if (index == last) {
      second = true;
      while (null != p && 0 == p.newObjects.size) {
        val n = p.next.asInstanceOf[Pool[_ <: T]]
        p = if (null != n && end < n.THH) n else null
      }
      if (null != p) {
        index = 0;
        last = p.newObjects.size
      }
    }
    r
  } else {

    val r = p.newObjects(index)
    index += 1
    if (index == last) {
      do {
        val n = p.next.asInstanceOf[Pool[_ <: T]]
        p = if (null != n && end < n.THH) n else null
      } while (null != p && 0 == p.newObjects.size)

      if (null != p) {
        index = 0;
        last = p.newObjects.size
      }
    }
    r
  }
}
