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

/**
 * Field types as used in reflective access.
 *
 * @author Timm Felden
 * @param <T>
 *            (boxed) runtime type of target objects
 */
abstract class FieldType[T](
  /**
   * the ID of this type (respective to the state it lives in)
   */
  val typeID : Int
) {
  /**
   * @return the human readable type name
   */
  override def toString : String;
}
