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
 * Follow the subtyping oriented variant of EnumProxies by Sarah Stieß. In contrast to her solution, we will only
 * instantiate this class directly if T is not contained in the tool specification.
 *
 * @author Timm Felden
 */
final class EnumProxy[T <: Enumeration](
  /**
   * Points to the target enum value, if it is part of the tool specification.
   *
   * @note null iff the target is not part of the tool specification.
   */
  val target : T#Value,

  /**
 * The name of this enum value. Names are stable.
 */
  val name : String,

  /**
 * The ID of this enum value. IDs are not stable as they depend on the input file.
 */
  val ID : Int
) {

  /**
   * The pool owning this enum value. It can be used to discover other enum values contained in this file.
   */
  private[internal] var _owner : EnumPool[T] = _
  def owner = _owner
}
