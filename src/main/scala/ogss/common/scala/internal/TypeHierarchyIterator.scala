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
 * iterates efficiently over the type hierarchy
 *
 * @author Timm Felden
 */
final class TypeHierarchyIterator[T <: Obj](
  private var p : Pool[_ <: T]
) extends Iterator[Pool[_ <: T]] {
  val end = p.THH

  override def hasNext = null != p

  override def next = {
    val r = p
    val n = p.next.asInstanceOf[Pool[_ <: T]]

    p = if (null != n && end < n.THH) n else null

    r
  }

  /**
   * @note valid, iff hasNext
   * @return the current element
   */
  def get = p
}
