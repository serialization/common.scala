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

import java.io.IOException
import java.nio.charset.Charset
import java.util.concurrent.Semaphore

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.global

import ogss.common.scala.api.InvalidPoolIndexException
import ogss.common.streams.BufferedOutStream
import ogss.common.streams.FileInputStream
import ogss.common.streams.FileOutputStream
import ogss.common.streams.MappedInStream

/**
 * @note String pools use magic index 0 for faster translation of string ids to strings.
 * @note String pool may contain duplicates, if strings have been added. This is a necessary behavior, if add should be
 *       an O(1) operation and Strings are loaded from file lazily.
 *
 * @author Timm Felden
 */
final class StringPool(

  /**
   * Strings used as names of types, fields or enum constants.
   *
   * @note literals are respective to the merged type system
   */
  private var literals : Array[String]

) extends HullType[String](9) {

  override def typeCheck(x : Any) : Boolean = x.isInstanceOf[String]

  /**
   * keep the mapped in stream open until all strings have been read from input HS
   */
  private var in : MappedInStream = null

  /**
   * ID ⇀ (absolute offset|32, length|32) will be used if idMap contains a null reference
   *
   * @note there is a fake entry at ID 0
   */
  private[internal] var positions : Array[Long] = null

  /**
   * Read the string literal block
   */
  private[internal] def readSL(in : FileInputStream) {
    val count = in.v32();
    if (0 == count) {
      // trivial merge
      return ;
    }

    // known/file literal index
    var ki = 0
    var fi = 0
    var next = new String(in.bytes(-1, in.v32()), StringPool.utf8);

    // merge literals from file into literals
    val merged = new ArrayBuffer[String](count);
    var hasFI = false
    var hasKI = false
    while ({
      hasFI = fi < count
      hasKI = ki < literals.length
      hasFI | hasKI
    }) {
      // note: we will intern the string only if it is unknown
      val cmp = if (hasFI) {
        if (hasKI) literals(ki).compareTo(next)
        else 1
      } else -1

      if (0 <= cmp) {
        if (0 == cmp) {
          // discard next
          next = literals(ki)
          ki += 1
        } else {
          // use next
          next = next.intern();
        }
        merged += next
        idMap += next

        fi += 1
        if (fi < count)
          next = new String(in.bytes(-1, in.v32()), StringPool.utf8);
      } else {
        merged += literals(ki)
        ki += 1
      }
    }

    // update literals if required
    if (literals.length != merged.size) {
      literals = merged.to
    }
  }

  /**
   * Read HS; we will not perform an actual read afterwards
   */
  protected[internal] override def allocateInstances(count : Int, in : MappedInStream) : Int = {
    this.in = in;

    // read offsets
    val offsets = new Array[Int](count)
    for (i ← 0 until count) {
      offsets(i) = in.v32();
    }

    // create positions
    var spi = idMap.size
    val sp = new Array[Long](spi + count)
    positions = sp;

    // store offsets
    // @note this has to be done after reading all offsets, as sizes are relative to that point and decoding
    // is done using absolute sizes
    var last = in.position()
    var len = 0
    for (i ← 0 until count) {
      len = offsets(i)
      sp(spi) = (last.toLong << 32L) | len.toLong;
      spi += 1
      idMap += null
      last += len;
    }

    return 0;
  }

  protected[internal] override def read(block : Int, map : MappedInStream) {
    // -done- strings are lazy
  }

  /**
   * The state will ask to drop the read buffer as soon as all strings must have been loaded, i.e. as soon as all
   * other lazy field data has been loaded.
   */
  private[internal] def dropRB {
    in = null;
    positions = null;
  }

  /**
   * write the string literal block to out and release the barrier when done, so that parallel creation of T and F can
   * be written to out
   *
   * @note the parallel write operation is synchronized on this, hence the buffer flush has to be synchronized on this
   *       as well
   */
  private[internal] def writeBlock(out : FileOutputStream) : Semaphore = {
    resetSerialization

    // create inverse map
    for (s ← literals) {
      IDs.put(s, idMap.size)
      idMap += s
    }

    val writeBarrier = new Semaphore(0, false);
    global.execute(new Runnable() {
      override def run = try {
        // count
        // @note idMap access performance hack
        val count = literals.length;
        out.v64(count);
        for (i ← 0 until count) {
          val img = literals(i).getBytes(StringPool.utf8);
          out.v64(img.length);
          out.put(img);
        }
      } catch {
        case e : IOException ⇒
          // should never happen!
          e.printStackTrace();
      } finally {
        writeBarrier.release();
      }
    })
    return writeBarrier;
  }

  /**
   * Write HS
   */
  protected[internal] override def write(block : Int, out : BufferedOutStream) : Boolean = {
    // the null in idMap is not written and literals are written in SL
    val hullOffset = literals.length + 1;
    val count = idMap.size - hullOffset;
    if (0 == count)
      return true;

    out.v64(count);

    // note: getBytes is an expensive operation!
    val images = new Array[Array[Byte]](count)
    // lengths
    for (i ← 0 until count) {
      val img = idMap(i + hullOffset).getBytes(StringPool.utf8);
      images(i) = img;
      out.v64(img.length);
    }

    // data
    for (i ← 0 until count) {
      out.put(images(i));
    }

    return false;
  }

  override def id(ref : String) : Int = if (null == ref) 0 else super.id(ref.intern())

  override def get(index : Int) : String = {
    if (0 == index)
      null
    else {
      // @note this block has to be synchronized in order to enable parallel
      // decoding of field data
      this.synchronized {
        var result = try {
          idMap(index)
        } catch {
          case e : IndexOutOfBoundsException ⇒
            throw new InvalidPoolIndexException(index, positions.length, "string", e);
        }
        if (null != result)
          return result;

        // we have to load the string from disk
        // @note this cannot happen if there was no HS, i.e. it is safe to access in & positions
        val off = positions(index)
        val chars = in.bytes((off >> 32L).toInt, off.toInt)

        result = new String(chars, StringPool.utf8).intern
        idMap(index) = result
        result
      }
    }
  }

  override def iterator = {
    val r = idMap.iterator
    // skip null-entry
    r.next
    r
  }

  private[internal] var _owner : State = _
  override def owner : State = _owner

  override val name = "string"
}

object StringPool {
  /**
   * cache the Charset used by OGSS
   */
  val utf8 = Charset.forName("UTF-8")
}
