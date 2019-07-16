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
 * The knowledge about the type system extracted from the specification in a
 * form that is suitable for efficient state initialization.
 *
 * @author Timm Felden
 */
abstract class PoolBuilder protected (

  /**
   * The size of SIFA as constructed by this PoolBuilder.
   */
  val sifaSize : Int
) {

  /**
   * In contrast to C++, we will directly return an array of strings. This is a sane solution because of String.intern
   * rules for literal strings in java. In consequence, by-name access is not required.
   *
   * @return The lexically sorted array of strings returned as name of a type, field or enum constant.
   */
  protected[internal] def literals : Array[String]

  /**
   * Known Container Constructor. Coded as kind|2 + sifaID|15 + sifaID|15. The IDs are relative to SIFA rather than
   * udts (note: has to include low IDs, i.e. sifaID is a shifted index)
   *
   * @return -1 if there are no more KCCs
   */
  protected[internal] def kcc(ID : Int) : Int

  /**
   * @return the name of the pool corresponding to the argument known id; return null if not a valid id
   */
  protected[internal] def name(ID : Int) : String

  /**
   * Create a new base pool.
   *
   * @return an instance of the pool corresponding to the argument known id
   */
  protected[internal] def make(ID : Int, index : Int) : Pool[_  <: Obj]

  /**
   * @return names of known enums in ascending order
   */
  protected[internal] def enumName(ID : Int) : String

  /**
   * @return values of known enums in ascending order
   */
  protected[internal] def enumMake(ID : Int) : Enumeration
}
