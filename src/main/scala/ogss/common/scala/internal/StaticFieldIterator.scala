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
 * Iterator over all fields declared by an access, i.e. excluding fields of
 * super types.
 *
 * @author Timm Felden
 */
final class StaticFieldIterator(
  private var p : Pool[_]
) extends Iterator[Field[_, _]] {

  private var i = -p.autoFields.length
  // find first valid state
  if (i == 0 && 0 == p.dataFields.size) {
    this.p = null
  }

  override def hasNext = p != null

  override def next : Field[_, _] = {
    val f : Field[_, _] =
      if (i < 0) p.autoFields(-1 - i)
      else p.dataFields(i)

    i += 1

    if (i == p.dataFields.size) {
      p = null
    }
    return f;
  }
}
