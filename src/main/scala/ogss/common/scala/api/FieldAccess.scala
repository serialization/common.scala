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
package ogss.common.scala.api

import ogss.common.scala.internal.Obj

/**
 * An abstract Field declaration, used for the runtime representation of types.
 * It can be used for reflective access of types.
 *
 * @author Timm Felden
 * @param <T>
 *            runtime type of the field modulo boxing
 */
trait FieldAccess[T] {
  /**
   * @return the type of this field
   */
  def t : FieldType[T]

  /**
   * @return OGSS name of this field
   */
  def name : String

  /**
   * @return enclosing type
   */
  def owner : GeneralAccess[_]

  /**
   * Generic getter for an object.
   *
   * @note it is up to the user to ensure that the field is valid for ref.
   */
  def get(ref : Obj) : T

  /**
   * Generic setter for an object.
   *
   * @note it is up to the user to ensure that the field is valid for ref.
   */
  def set(ref : Obj, value : T) : Unit
}
