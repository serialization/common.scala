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

import ogss.common.scala.internal.State

/**
 * Access to class type T
 *
 * @author Timm Felden
 */
trait GeneralAccess[T] extends FieldType[T] with Iterable[T] {

  /**
   * @return the OGSS name of the type
   */
  val name : String

  /**
   * @return the file owning this access
   */
  def owner : State

  /**
   * @return the number of objects returned by the default iterator
   */
  def size : Int

  /**
   * get an instance by its ID
   *
   * @note This is only usable for instances with IDs and for valid IDs. This function is unrelated to Collection.get
   */
  def get(ID : Int) : T

  final override def toString : String = name
}
