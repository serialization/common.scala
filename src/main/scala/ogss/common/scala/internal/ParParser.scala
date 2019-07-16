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

import ogss.common.streams.FileInputStream
import java.util.concurrent.Semaphore
import scala.concurrent.ExecutionContext.global
import scala.collection.mutable.ArrayBuffer
import ogss.common.streams.MappedInStream
import ogss.common.scala.Constants
import java.nio.BufferUnderflowException
import ogss.common.scala.api.PoolSizeMissmatchError
import ogss.common.scala.api.OGSSException

/**
 * The parallel version of Parser.
 *
 * @author Timm Felden
 */
final class ParParser(
  _in : FileInputStream,
  _pb : PoolBuilder,

  // synchronization of field read jobs
  val barrier : Semaphore = new Semaphore(0),

  // jobs is a field as we need it for await
  var jobs : ArrayBuffer[Runnable] = null
) extends Parser(_in, _pb) {

  /**
   * parse T and F
   */
  override def typeBlock {
    // ensure correct initialization order
    assert(null != barrier)

    /**
     * *************** * T Class * ****************
     */
    typeDefinitions

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
            global.execute(new Runnable() {
              override def run {
                p.allocateInstances
                barrier.release();
              }
            });
          } else {
            // we would not allocate an instance anyway
            barrier.release();
          }
        }
      }
    }

    /**
     * *************** * T Container * ****************
     */
    TContainer

    /**
     * *************** * T Enum * ****************
     */
    TEnum

    /**
     * *************** * F * ****************
     */
    for (p ← classes) {
      readFields(p)
    }
  }

  /**
   * Jump through HD-entries to create read tasks
   */
  override def processData {

    // we expect one HD-entry per field
    jobs = new ArrayBuffer(fields.size)

    var awaitHulls = 0;

    // use a big lock on jobs, because we are usually the only ones who insert anyway
    jobs.synchronized {
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
            awaitHulls += 1
            global.execute(new Runnable() {
              override def run() {
                val block = p.allocateInstances(count, map);

                // create hull read data task except for StringPool which is still lazy per element and
                // eager per offset
                if (!p.isInstanceOf[StringPool]) {
                  // @note modification of the job queue requires synchronization
                  jobs.synchronized {
                    jobs += new HRT(p, block, map)
                  }
                }

                barrier.release();
              }
            });

          }
          case fd : Field[_, _] ⇒ {
            // create job with adjusted size that corresponds to the * in the specification (i.e. exactly the
            // data)
            jobs += new ReadTask(fd, map)
          }
        }
      }
    }

    // await allocations of class and hull types
    barrier.acquire(classes.size + awaitHulls);

    // start read tasks
    for (j ← jobs)
      global.execute(j);

    // TODO start tasks that perform default initialization of fields not obtained from file
  }

  override def awaitResults {
    // await read jobs and throw error if any occurred
    barrier.acquire(jobs.size);

    if (null != readErrors)
      throw readErrors;
  }

  private final class ReadTask(
    private val f :  Field[_, _],
    private val in : MappedInStream
  ) extends Runnable {

    override def run {
      if (in.eof()) {
        // TODO default initialization; this is a nop for now in Java
        barrier.release();
        return ;
      }
      val block = if (f.owner.cachedSize >= Constants.FD_Threshold) in.v32 else 0

      var ex : OGSSException = null
      val owner = f.owner;
      val bpo = owner.bpo;
      val first = block * Constants.FD_Threshold;
      val last = Math.min(owner.cachedSize, first + Constants.FD_Threshold);
      try {
        f.read(bpo + first, bpo + last, in);

        if (!in.eof() && !f.isInstanceOf[LazyField[_, _]])
          ex = new PoolSizeMissmatchError(in.position(), bpo + first, bpo + last, f);

      } catch {
        case e : BufferUnderflowException ⇒ ex = new PoolSizeMissmatchError(bpo + first, bpo + last, f, e);
        case t : OGSSException            ⇒ ex = t;
        case t : Throwable                ⇒ ex = new OGSSException("internal error: unexpected foreign exception", t);
      } finally {
        barrier.release();
        if (null != ex)
          fields.synchronized {
            if (null == readErrors)
              readErrors = ex;
            else
              readErrors.addSuppressed(ex);
          }
      }
    }
  }

  /**
   * A hull read task. Reads H-Data.
   *
   * @author Timm Felden
   */
  private final class HRT(
    private val t :     HullType[_],
    private val block : Int,
    private val map :   MappedInStream
  ) extends Runnable {

    override def run {
      var ex : OGSSException = null;
      try {
        t.read(block, map);
      } catch {
        case t : OGSSException ⇒ ex = t;
        case t : Throwable     ⇒ ex = new OGSSException("internal error: unexpected foreign exception", t);
      } finally {
        barrier.release();
        if (null != ex)
          fields.synchronized {
            if (null == readErrors)
              readErrors = ex;
            else
              readErrors.addSuppressed(ex);
          }
      }
    }
  }
}
