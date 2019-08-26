/** *****************************************************************************
 * Copyright 2019 University of Stuttgart
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License.  You may obtain a copy
 * of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
 * License for the specific language governing permissions and limitations under
 * the License.
 * *****************************************************************************/
package ogss.common.scala.internal

import java.util
import java.util.ArrayList

import ogss.common.streams.FileOutputStream
import ogss.common.streams.OutStream
import java.util.concurrent.Semaphore

import ogss.common.streams.BufferedOutStream
import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.ExecutionContext.global
import scala.collection.mutable.ArrayBuffer
import ogss.common.scala.internal.fieldTypes.SingleArgumentType
import ogss.common.scala.internal.fieldTypes.MapType
import ogss.common.scala.api.OGSSException
import ogss.common.scala.Constants
import ogss.common.scala.internal.fieldTypes.SingleArgumentType
import ogss.common.scala.internal.fieldTypes.MapType
import ogss.common.scala.internal.fieldTypes.ContainerType

/**
 * Writes a state to disk.
 *
 * @author Timm Felden
 */
final class Writer(
                    val state: State,
                    out: FileOutputStream
                  ) {

  import Writer._

  /**
   * The outgoing file type ids.
   * There is one entry per TID in the runtime type system.
   *
   * @note this table is required, because we can drop types and we want the runtime TIDs to be immutable unique and ordered
   */
  private var FTID = new Array[Int](10 + state.classes.length + state.containers.length + state.enums.length);
  for (i <- 0 until 10) FTID(i) = i

  /**
   * This FTID is assigned to a type when it introduces a new TID in the outgoing type system
   */
  private var nextFTID = 10

  /**
   * The outgoing file field IDs.
   */
  private[internal] var FFID: Array[Int] = {
    //next FFID, because 0 is always the string pool, even if not used later on
    var nextFFID: Int = 1
    // we do not keep a last FFID, so we have to create an array buffer first
    val ids = new ArrayBuffer[Int]
    // string -> string
    ids += 0
    // containers
    for (c <- state.containers) {
      ids += (if (c.maxDeps == 0) -1
      else {
        nextFFID += 1;
        nextFFID - 1
      })
    }
    // data fields
    for (p <- state.classes) {
      for (f <- p.dataFields) {
        assert(f.ID == ids.size)
        ids += nextFFID
        nextFFID += 1
      }
    }

    ids.toArray[Int]
  }


  // async reads will post their errors in this queue
  var writeErrors: Throwable = null;

  // our job synchronization barrier
  val barrier = new Semaphore(0, false);

  /**
   * the number of buffers that will be sent to the write job; synchronize on this to protect it on modification
   */
  var awaitBuffers = 0;

  // @note can be used to add buffers concurrently to the write queue
  // @note the permit is given after we added a buffer; therefore the reader
  // can always read some buffer if he uses our permit (order is not
  // important)
  val finishedBuffers = new ConcurrentLinkedQueue[BufferedOutStream]
  val recycleBuffers = new ConcurrentLinkedQueue[BufferedOutStream]

  /**
   * write T and F, start HD tasks and set awaitBuffers to the number of buffers if every entry had one block
   */
  private def writeTF(out: BufferedOutStream) {

    var awaitHulls = 0;

    /**
     * *************** * T Class * ****************
     */

    // calculate new bpos, sizes, object IDs and compress data arrays
    val bases = {
      val bpos = new Array[Int](state.classes.length)
      var bases = 0;
      for (p ← state.classes) {
        if (null == p.superPool) {
          bases += 1
          global.execute(new WCompress(this, p, bpos));
        }
      }
      bases
    }

    // write count of the type block
    out.v64(state.classes.length);

    // initialize local state before waiting for compress
    val fieldQueue = new ArrayBuffer[Field[_, _]](2 * state.classes.length);
    val string = state.strings;

    barrier.acquire(bases);

    // write types
    for (p ← state.classes) {
      FTID(p.typeID) = nextFTID
      nextFTID += 1

      out.v64(string.IDs.get(p.name));
      out.v64(p.staticDataInstances);
      attributes(p, out);
      if (null == p.superPool)
        out.i8(0);
      else {
        // superID
        out.v64(p.superPool.typeID - 9);
        // our bpo
        out.v64(p.bpo);
      }

      out.v64(p.dataFields.size);

      // add field to queues for description and data tasks
      for (f ← p.dataFields) {
        fieldQueue += f
      }
    }

    /**
     * *************** * T Container * ****************
     */

    // write count of the type block
    {
      var count = 0;
      // set deps and calculate count
      for (c ← state.containers) {
        if (c.maxDeps != 0) {
          c.resetSerialization
          c.deps = c.maxDeps;
          count += 1
        }
      }
      if (string.maxDeps != 0) {
        awaitHulls = 1;
        string.deps = string.maxDeps;
      }
      awaitHulls += count;

      out.v64(count);
      for (c ← state.containers) {
        if (c.maxDeps != 0) {
          FTID(c.typeID) = nextFTID
          nextFTID += 1

          c match {
            case t: SingleArgumentType[_, _] ⇒
              out.i8(t.kind);
              out.v64(FTID(t.base.typeID));

            case t: MapType[_, _] ⇒
              out.i8(3.toByte);
              out.v64(FTID(t.keyType.typeID));
              out.v64(FTID(t.valueType.typeID));
          }
        }
      }
    }

    // note: we cannot start field jobs immediately because they could decrement deps to 0 multiple times in that
    // case
    for (f ← fieldQueue) {
      global.execute(new WFT(this, f));
    }

    /**
     * *************** * T Enum * ****************
     */

    // write count of the type block
    out.v64(state.enums.length);
    for (p ← state.enums) {
      FTID(p.typeID) = nextFTID
      nextFTID += 1

      out.v64(string.id(p.name));
      out.v64(p.values.length);
      for (v ← p.values) {
        out.v64(string.id(v.name));
      }
    }

    /**
     * *************** * F * ****************
     */

    for (f ← fieldQueue) {
      // write info
      out.v64(string.id(f.name));
      out.v64(FTID(f.t.typeID));
      attributes(f, out);
    }

    out.close();

    // fields + hull types
    synchronized {
      awaitBuffers += (fieldQueue.size + awaitHulls);
    }
  }

  {
    /**
     * *************** * G * ****************
     */
    if (null == state.guard || state.guard.isEmpty()) {
      out.i16(0x2622.toShort);
    } else {
      out.i8('#'.toByte);
      out.put(state.guard.getBytes());
      out.i8(0.toByte);
    }

    /**
     * *************** * S * ****************
     */

    // our string synchronisation barrier
    val SB = state.strings.writeBlock(out);

    /**
     * *************** * T F * ****************
     */

    // write T and F to a buffer, while S is written
    val buffer = new BufferedOutStream();

    // @note here, the field data write tasks will be started already
    writeTF(buffer);
    SB.acquire();

    // write buffered TF-blocks
    out.write(buffer);
    recycleBuffers.add(buffer);

    /**
     * *************** * HD * ****************
     */

    // await data from all HD tasks
    while (synchronized {
      awaitBuffers -= 1
      0 <= awaitBuffers
    }) {
      barrier.acquire();
      val buf = finishedBuffers.poll();
      if (null != buf) {
        out.writeSized(buf);
        recycleBuffers.add(buf);
      }
      // else: some buffer was discarded
    }

    out.close();

    // check internal state
    // all containers have been written or were not used
    for (c ← state.containers if c.deps != 0) {
      val e = new InternalError(s"$c has ${c.deps} of ${c.maxDeps}")
      if (writeErrors != null)
        writeErrors.addSuppressed(e)
      else
        writeErrors = e
    }

    // report errors
    if (null != writeErrors) {
      throw new OGSSException("write failed", writeErrors);
    }
  }
}

object Writer {

  /**
   * TODO serialization of attributes
   */
  def attributes(p: Pool[_], out: OutStream) {
    out.i8(0.toByte);
  }

  /**
   * TODO serialization of attributes
   */
  def attributes(f: Field[_, _], out: OutStream) {
    out.i8(0.toByte)
  }
}

/**
 * A job that performs the (former) compress operation on a base pool.
 *
 * @author Timm Felden
 */
final class WCompress(
                       val self: Writer,
                       val base: Pool[_ <: Obj],
                       val bpos: Array[Int]
                     ) extends Runnable {

  /**
   * compress new instances into the data array and update object IDs
   */
  override def run {

    // create our part of the bpo map
    {
      var next = 0;
      var p = base;

      do {
        bpos(p.typeID - 10) = next;
        val s = p.staticSize - p.deletedCount;
        p.cachedSize = s;
        next += s;
        p = p.next
      } while (null != p);
    }

    // calculate correct dynamic size for all sub pools
    {
      val cs = base.owner.classes
      var i = cs.length - 1
      var p = cs(i)
      while (base != p) {
        if (base == p.basePool) {
          p.superPool.cachedSize += p.cachedSize;
        }
        i -= 1
        p = cs(i)
      }
    }

    // reset layout of distributed fields
    {
      var p = base;
      while (null != p) {
        for (f ← p.dataFields) {
          f match {
            case f: DistributedField[_, _] ⇒ f.compress(bpos(p.typeID - 10))
            case _ ⇒
          }
        }
        p = p.next;
      }
    }

    // note: we could move the object update to updateAfterCompress and
    // perform that in parallel (because it is much easier without append)

    // from now on, size will take deleted objects into account, thus d may
    // in fact be smaller then data!
    val d = new Array[Obj](base.cachedSize)
    var pos = 0;
    val is = new TypeOrderIterator(base);
    while (is.hasNext) {
      val i = is.next
      if (i._ID != 0) {
        d(pos) = i;
        pos += 1
        i._ID = pos;
      }
    }

    // update after compress for all sub-pools
    var p = base;

    do {
      // update data
      p.asInstanceOf[Pool[Obj]].data = d;

      // update structural knowledge of data
      p.staticDataInstances += p.newObjects.size - p.deletedCount;
      p.deletedCount = 0;
      p.newObjects.clear

      p.bpo = bpos(p.typeID - 10)

      p = p.next;
    } while (null != p);

    self.barrier.release();
  }
}

/**
 * A Writer Job.
 *
 * @author Timm Felden
 */
sealed abstract class WJob(
                            val self: Writer
                          ) extends Runnable {

  var discard = true

  var tail: WJob = null

  override def run {
    var buffer = self.recycleBuffers.poll();
    if (null == buffer) {
      buffer = new BufferedOutStream();
    } else {
      buffer.recycle();
    }

    try {
      job(buffer);
    } catch {
      case e: Throwable ⇒
        self.synchronized {
          if (null == self.writeErrors)
            self.writeErrors = e;
          else
            self.writeErrors.addSuppressed(e);
        }
    } finally {
      // return the buffer in any case to ensure that there is a
      // buffer on error
      buffer.close();
      if (discard) {
        self.recycleBuffers.add(buffer);
      } else {
        self.finishedBuffers.add(buffer);
      }

      // ensure that writer can terminate, errors will be
      // printed to command line anyway, and we wont
      // be able to recover, because errors can only happen if
      // the OGSS implementation itself is broken
      self.barrier.release()

      if (null != tail)
        tail.run();
    }
  }

  protected def job(buffer: BufferedOutStream)
}

/**
 * A job that writes field data to a buffer.
 *
 * @author Timm Felden
 */
final class WFT(
                 _self: Writer,
                 val f: Field[_, _]
               ) extends WJob(_self) {

  /**
   * the block this task is responsible for; the task processing block 0 starts the other tasks and can therefore
   * know that it is not just a task that has to process its block
   */
  private var block = 0

  override def job(buffer: BufferedOutStream) {

    val size = f.owner.cachedSize

    var hasblocks = false

    // any empty field will be discarded
    if (size != 0) {

      // iff we have blockID zero we may need to split
      if (0 == block) {
        // split large FD blocks into blocks
        if (size > Constants.FD_Threshold) {
          hasblocks = true;

          // we have to fork this task
          var blockCount = (size - 1) / Constants.FD_Threshold;
          // @note we increment await by blockCount - 1
          self.synchronized(self.awaitBuffers += blockCount)
          blockCount += 1

          f.blocks = blockCount;
          var i = 1
          while (i < blockCount) {
            val job = new WFT(self, f);
            job.block = i;
            global.execute(job);
            i += 1
          }
        }
      } else {
        hasblocks = true;
      }

      val owner = f.owner;
      val bpo = owner.bpo;
      var i = block * Constants.FD_Threshold;
      var h = Math.min(size, i + Constants.FD_Threshold);
      i += bpo;
      h += bpo;

      buffer.v64(self.FFID(f.ID));
      if (size > Constants.FD_Threshold) {
        buffer.v64(block);
      }
      discard = f.write(i, h, buffer);

    } else {
      hasblocks = false;
    }

    val done =
      if (hasblocks) f.synchronized({
        f.blocks -= 1
        0 == f.blocks
      })
      else true

    if (done) {
      f.t match {
        case t: HullType[_] ⇒ t.synchronized {
          t.deps -= 1
          assert(t.deps >= 0)
          if (0 == t.deps) {
            // execute task in this thread to avoid unnecessary overhead
            tail = new WHT(self, t);
          }
        }
        case _ ⇒
      }
    }
  }
}

/**
 * A job that writes a hull type to a buffer.
 *
 * @author Timm Felden
 */
final class WHT(
                 _self: Writer,
                 val ht: HullType[_]
               ) extends WJob(_self) {

  /**
   * the block this task is responsible for; the task processing block 0 starts the other tasks and can therefore know
   * that it is not just a task that has to process its block
   */
  private var block = 0

  override def job(buffer: BufferedOutStream) {
    ht match {
      case t: ContainerType[_] ⇒ {
        var hasblocks = false
        val size = t.IDs.size
        discard = 0 == size

        if (!discard) {
          // iff we have blockID zero we may need to split
          if (0 == block) {
            // split non-HS blocks that are too large into blocks
            if (size > Constants.HD_Threshold) {
              hasblocks = true;
              // we have to fork this task
              var blockCount = (size - 1) / Constants.HD_Threshold;
              // @note we increment await by blockCount - 1
              self.synchronized(self.awaitBuffers += blockCount)
              blockCount += 1

              t.blocks = blockCount;
              var i = 1
              while (i < blockCount) {
                val job = new WHT(self, t);
                job.block = i;
                global.execute(job);
                i += 1
              }
            }
          } else {
            hasblocks = true;
          }

          buffer.v64(self.FFID(t.fieldID));
          buffer.v64(size);
          if (size > Constants.HD_Threshold) {
            buffer.v64(block);
          }
          val i = block * Constants.HD_Threshold;
          val end = Math.min(size, i + Constants.HD_Threshold);
          t.write(i, end, buffer);
        }

        val done =
          if (hasblocks) t.synchronized({
            t.blocks -= 1
            0 == t.blocks
          })
          else true

        if (done) {
          ht match {
            case ct: SingleArgumentType[_, _] ⇒ ct.base match {
              case bt: HullType[_] ⇒ bt.synchronized {
                bt.deps -= 1
                assert(bt.deps >= 0)
                if (0 == bt.deps) {
                  // execute task in this thread to avoid unnecessary overhead
                  tail = new WHT(self, bt);
                }
              }
              case _ ⇒
            }

            case ct: MapType[_, _] ⇒ {
              ct.keyType match {
                case bt: HullType[_] ⇒ bt.synchronized {
                  bt.deps -= 1
                  assert(bt.deps >= 0)
                  if (0 == bt.deps) {
                    // execute task in this thread to avoid unnecessary overhead
                    global.execute(new WHT(self, bt))
                  }
                }
                case _ ⇒
              }
              ct.valueType match {
                case bt: HullType[_] ⇒ bt.synchronized {
                  bt.deps -= 1
                  if (0 == bt.deps) {
                    // execute task in this thread to avoid unnecessary overhead
                    tail = new WHT(self, bt);
                  }
                }
                case _ ⇒
              }
            }
            case _ ⇒
          }
        }
      }
      case t: StringPool ⇒ discard = t.write(buffer)
    }
  }
}
