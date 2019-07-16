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
 * Iterates efficiently over dynamic instances of a pool in type order.
 *
 * @author Timm Felden
 * @note cast required to work around weakened type system by javac 1.8.131
 */
final class TypeOrderIterator[T <: Obj](_p : Pool[T]) extends Iterator[T] {
  val ts = new TypeHierarchyIterator(_p)
  var is = {
    while (ts.hasNext && 0 == ts.get.staticSize) {
      ts.next
    }
    if (ts.hasNext)
      new StaticDataIterator(ts.get)
    else
      null
  }

  override def hasNext = null != is

  override def next = {
    val r = is.next
    if (!is.hasNext) {
      is = {
        while (ts.hasNext && 0 == ts.get.staticSize) {
          ts.next
        }
        if (ts.hasNext)
          new StaticDataIterator(ts.get)
        else
          null
      }
    }
    r
  }
}
