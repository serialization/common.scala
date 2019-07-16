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
package ogss.common.scala.internal.fields

import ogss.common.scala.internal.Field
import ogss.common.scala.internal.FieldType
import ogss.common.scala.internal.Obj
import ogss.common.scala.internal.Pool
import ogss.common.streams.BufferedOutStream
import ogss.common.streams.MappedInStream

/**
 * This trait marks auto fields.
 *
 * @author Timm Felden
 */
abstract class AutoField[T, Ref <: Obj](
  _t :     FieldType[T],
  _name :  String,
  _index : Int,
  _owner : Pool[Ref]
) extends Field[T, Ref](_t, _name, _index, _owner)
  with KnownField {

  final override def read(i : Int, h : Int, in : MappedInStream) =
    throw new NoSuchMethodError("one cannot read auto fields!")

  final override def write(i : Int, h : Int, out : BufferedOutStream) =
    throw new NoSuchMethodError("one cannot write auto fields!")
}
