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

import ogss.common.jvm.streams.FileInputStream
import scala.collection.mutable.ArrayBuffer
import ogss.common.scala.Constants
import ogss.common.scala.api.OGSSException
import ogss.common.jvm.streams.MappedInStream
import ogss.common.scala.api.PoolSizeMissmatchError
import java.nio.BufferUnderflowException
import ogss.common.scala.internal.fieldTypes.ContainerType

/**
 * The sequential version of Parser.
 *
 * @author Timm Felden
 */
final class SeqParser(
  _in : FileInputStream,
  _pb : PoolBuilder
) extends Parser(_in, _pb) {

  /**
   * parse T and F
   */
  override def typeBlock {

    /**
     * *************** * T Class * ****************
     */
    {
      typeDefinitions
    };

    // calculate cached size and next for all pools
    {
      val cs = classes.size
      if (0 != cs) {
        var i = cs - 2;
        if (i >= 0) {
          var n : Pool[_] = null
          var p = classes(i + 1);
          // propagate information in reverse order
          // i is the pool where next is set, hence we skip the last pool
          do {
            n = p;
            p = classes(i);

            // by compactness, if n has a super pool, p is the previous pool
            if (null != n.superPool) {
              n.superPool.cachedSize += n.cachedSize;
            }

            i -= 1
          } while (i >= 0);
        }

        // allocate data and start instance allocation jobs
        var d : Array[Obj] = null
        while ({
          i += 1
          i < cs
        }) {
          val p = classes(i);
          if (null == p.superPool) {
            // create new d, because we are in a new type hierarchy
            d = new Array[Obj](p.cachedSize)
          }
          p.asInstanceOf[Pool[Obj]].data = d
          if (0 != p.staticDataInstances) {
            p.allocateInstances
          }
        }
      }
    };

    /**
     * *************** * T Container * ****************
     */
    {
      TContainer
    };

    /**
     * *************** * T Enum * ****************
     */
    {
      TEnum
    };

    assert(SIFA.forall(null != _))

    /**
     * *************** * F * ****************
     */
    for (p ← classes) {
      readFields(p.asInstanceOf[Pool[Obj]]);
    }
  }

  /**
   * Jump through HD-entries to create read tasks
   */
  override def processData {

    // we expect one HD-entry per field
    val jobs = new ArrayBuffer[Job](fields.size)

    while (!in.eof()) {
      // create the map directly and use it for subsequent read-operations to avoid costly position and size
      // readjustments
      val map = in.map(in.v32() + 2);

      val id = map.v32();
      // TODO add a countermeasure against duplicate buckets / fieldIDs

      fields(id) match {
        case p : HullType[_] ⇒ {
          val count = map.v32();

          // start hull allocation job
          val block = p.allocateInstances(count, map);

          // create hull read data task except for StringPool which is still lazy per element and eager per offset
          p match {
            case p : ContainerType[_] ⇒
              // @note modification of the job queue requires synchronization
              jobs.synchronized {
                jobs += new HRT(p, block, map)
              }
            case _ ⇒
          }

        }
        case fd : Field[_, _] ⇒ {
          val block = if (fd.owner.cachedSize > Constants.FD_Threshold) map.v32() else 0

          // create job with adjusted size that corresponds to the * in the specification (i.e. exactly the data)
          jobs += new ReadTask(block, fd, map)
        }
      }
    }

    // perform read tasks
    try {
      for (j ← jobs)
        j.run
    } catch {
      case t : OGSSException ⇒ throw t
      case t : Throwable     ⇒ throw new OGSSException("internal error: unexpected foreign exception", t)
    }

    // TODO start tasks that perform default initialization of fields not obtained from file
  }

  private final class ReadTask(
    private val block : Int,
    private val f :     Field[_, _],
    private val in :    MappedInStream
  ) extends Job {

    override def run {
      val owner = f.owner;
      val bpo = owner.bpo;
      val first = block * Constants.FD_Threshold;
      val last = Math.min(owner.cachedSize, first + Constants.FD_Threshold);
      try {
        f.read(bpo + first, bpo + last, in);

        if (!in.eof() && !f.isInstanceOf[LazyField[_, _]])
          throw new PoolSizeMissmatchError(in.position(), bpo + first, bpo + last, f);

      } catch {
        case e : BufferUnderflowException ⇒
          throw new PoolSizeMissmatchError(bpo + first, bpo + last, f, e);
      }
    }
  }

  private final class HRT(
    private val t :     ContainerType[_],
    private val block : Int,
    private val map :   MappedInStream
  ) extends Job {

    override def run {
      val i = block * Constants.HD_Threshold;
      val end = Math.min(t.idMap.size - 1, i + Constants.HD_Threshold)
      t.read(i, end, map);
    }
  }
}
