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
package ogss.common.scala.internal.restrictions

/**
 * A restriction that can be applied to a field.
 *
 * TODO should be renamed to attribute
 *
 * @author Timm Felden
 * @param <T>
 *            The Scala type of the field.
 */
abstract class FieldRestriction[T] {

  /**
   * Checks a value and throws an exception in case of error. We prefer the
   * exception throwing mechanism over return values, because we expect checks
   * to fail almost never.
   *
   * @param value
   *            the value to be checked
   */
  def check(value : T) : Unit
}
