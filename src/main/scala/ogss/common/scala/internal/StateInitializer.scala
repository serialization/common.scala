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

import java.nio.BufferUnderflowException
import java.nio.file.Path

import scala.collection.mutable.ArrayBuffer

import ogss.common.scala.api.Mode
import ogss.common.scala.api.OGSSException
import ogss.common.scala.internal.fieldTypes.Bool
import ogss.common.scala.internal.fieldTypes.ContainerType
import ogss.common.scala.internal.fieldTypes.F32
import ogss.common.scala.internal.fieldTypes.F64
import ogss.common.scala.internal.fieldTypes.I16
import ogss.common.scala.internal.fieldTypes.I32
import ogss.common.scala.internal.fieldTypes.I64
import ogss.common.scala.internal.fieldTypes.I8
import ogss.common.scala.internal.fieldTypes.MapType
import ogss.common.scala.internal.fieldTypes.SingleArgumentType
import ogss.common.scala.internal.fieldTypes.V64
import ogss.common.jvm.streams.FileInputStream

/**
 * Initializes a state. One of Creator, Parser, SequentialParser.
 *
 * @author Timm Felden
 */
abstract class StateInitializer protected (
  pb : PoolBuilder
) {

  var path : Path = _
  var canWrite : Boolean = _

  // guard from file
  var guard : String = _

  // strings
  val strings = new StringPool(pb.literals)

  // types
  val classes = new ArrayBuffer[Pool[_ <: Obj]]
  val containers = new ArrayBuffer[ContainerType[_]]
  val enums = new ArrayBuffer[EnumPool[_ <: Enumeration]]
  val anyRef = new AnyRefType(classes)

  /**
   * State Initialization of Fields Array.
   *
   * @note invariant: ∀i. SIFA[i].name == pb.KCN(i)
   */
  val SIFA = new Array[FieldType[_]](pb.sifaSize)

  SIFA(0) = Bool
  SIFA(1) = I8
  SIFA(2) = I16
  SIFA(3) = I32
  SIFA(4) = I64
  SIFA(5) = V64
  SIFA(6) = F32
  SIFA(7) = F64
  SIFA(8) = anyRef
  SIFA(9) = strings

  /**
   * next SIFA ID to be used if some type is added to SIFA
   */
  var nsID = 10

  /**
   * The next global field ID. Note that this ID does not correspond to the ID used in the file about to be read but
   * to an ID that would be used if it were written.
   *
   * @note to make this work as intended, merging known fields into the dataFields array has to be done while reading
   *       F.
   * @note ID 0 is reserved for the String hull which is always present
   */
  protected var nextFieldID = 1;

  /**
   * Calculate correct maxDeps values for containers used by containers.
   */
  protected def fixContainerMD {
    // increase deps caused by containsers whose maxDeps is nonzero
    // @note we have to increase deps in reverse order, because used container containers appear after their bases
    var i = containers.size
    while (0 != i) {
      i -= 1
      val c = containers(i)
      if (c.maxDeps != 0) {
        c match {
          case c : SingleArgumentType[_, _] ⇒ {
            c.base match {
              case b : HullType[_] ⇒ b.maxDeps += 1
              case _               ⇒
            }
          }
          case c : MapType[_, _] ⇒ {
            c.keyType match {
              case b : HullType[_] ⇒ b.maxDeps += 1
              case _               ⇒
            }
            c.valueType match {
              case b : HullType[_] ⇒ b.maxDeps += 1
              case _               ⇒
            }
          }
          case _ ⇒
        }
      }
    }
  }

  /**
   * Called by the concrete state before returning from the constructor to ensure that potentially running parallel
   * tasks finished.
   */
  def awaitResults {
    // nothing by default
  }
}

object StateInitializer {
  def apply(path : Path, pb : PoolBuilder, mode : Seq[Mode]) : StateInitializer = {
    val modes = new ActualMode(mode);
    val init =
      if (modes.create) {
        new Creator(pb);
      } else {
        val fs = FileInputStream.open(path);
        try {
          if (fs.size < Parser.SEQ_LIMIT)
            new SeqParser(fs, pb);
          else
            new ParParser(fs, pb);
        } catch {
          case e : BufferUnderflowException ⇒
            throw new OGSSException("unexpected EOF", e)
        }
      }
    init.path = path;
    init.canWrite = modes.write;
    return init;
  }
}
