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
import ogss.common.scala.internal.StaticFieldIterator
import ogss.common.scala.internal.FieldIterator
import ogss.common.scala.internal.DynamicDataIterator
import ogss.common.scala.internal.TypeOrderIterator

/**
 * Access to class type T
 *
 * @author Timm Felden
 */
trait Access[T <: Obj] extends GeneralAccess[T] {

  /**
   * the SKilL name of the super type of T, if any
   */
  def superType : Access[_ >: T <: Obj]

  /**
   * @return a new T instance with default field values
   */
  def make : T

  /**
   * just for convenience
   */
  def iterator : DynamicDataIterator[T]

  /**
   * @return a type ordered Container iterator over all instances of T
   */
  def inTypeOrder : TypeOrderIterator[T]

  /**
   * @return an iterator over all field declarations, even those provided by the binary skill file
   */
  def fields : StaticFieldIterator[T]

  /**
   * @return an iterator over all field declarations, even those provided by the binary skill file, including fields
   * declared in super types
   */
  def allFields : FieldIterator
}
