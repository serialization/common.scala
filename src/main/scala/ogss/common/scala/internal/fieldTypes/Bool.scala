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
package ogss.common.scala.internal.fieldTypes

import ogss.common.streams.MappedInStream
import ogss.common.streams.InStream
import ogss.common.scala.internal.FieldType
import ogss.common.streams.OutStream

/**
 * @author Timm Felden
 */
case object Bool extends FieldType[Boolean](0) {

  override def r(in : InStream) = in.asInstanceOf[MappedInStream].bool

  override def w(v : Boolean, out : OutStream) =
    throw new NoSuchMethodError("the caller has to wrap out!")
  
  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[Boolean]

  override def toString = "bool"
}
