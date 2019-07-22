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

import java.io.ByteArrayOutputStream
import java.util.IdentityHashMap

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import ogss.common.scala.api.OGSSException
import ogss.common.scala.api.ParseException
import ogss.common.scala.internal.fieldTypes.ArrayType
import ogss.common.scala.internal.fieldTypes.Bool
import ogss.common.scala.internal.fieldTypes.ContainerType
import ogss.common.scala.internal.fieldTypes.F32
import ogss.common.scala.internal.fieldTypes.F64
import ogss.common.scala.internal.fieldTypes.I16
import ogss.common.scala.internal.fieldTypes.I32
import ogss.common.scala.internal.fieldTypes.I64
import ogss.common.scala.internal.fieldTypes.I8
import ogss.common.scala.internal.fieldTypes.ListType
import ogss.common.scala.internal.fieldTypes.MapType
import ogss.common.scala.internal.fieldTypes.SetType
import ogss.common.scala.internal.fieldTypes.V64
import ogss.common.scala.internal.fields.AutoField
import ogss.common.scala.internal.restrictions.FieldRestriction
import ogss.common.scala.internal.restrictions.TypeRestriction
import ogss.common.streams.FileInputStream

/**
 * Strategy-independent parser functionality.
 *
 * @author Timm Felden
 */
abstract class Parser(
  val in :           FileInputStream,
  protected val pb : PoolBuilder
) extends StateInitializer(pb) {

  /**
   * This buffer provides the association of file fieldID to field.
   */
  protected val fields = new ArrayBuffer[AnyRef]

  /**
   * File defined types. This array is used to resolve type IDs while parsing. The type IDs assigned to created
   * entities may not correspond to fdts indices (shifted by 10).
   */
  private[internal] val fdts = new ArrayBuffer[FieldType[_]]

  var readErrors : OGSSException = null;

  /**
   * Turns a field type into a preliminary type information. In case of user types, the declaration of the respective
   * user type may follow after the field declaration.
   */
  private final def fieldType() : FieldType[_] = {
    in.v32() match {
      case 0      ⇒ Bool
      case 1      ⇒ I8
      case 2      ⇒ I16
      case 3      ⇒ I32
      case 4      ⇒ I64
      case 5      ⇒ V64
      case 6      ⇒ F32
      case 7      ⇒ F64
      case 8      ⇒ anyRef;
      case 9      ⇒ strings;
      case typeID ⇒ fdts(typeID - 10);
    }
  }

  final def typeAttribute(count : Int) : HashSet[TypeRestriction[_]] = {
    val r = new HashSet[TypeRestriction[_]]
    // parse count many entries
    var i = count
    while (i != 0) {
      i -= 1
      throw new ParseException(s"Serialized type attributes are currently unsupported.", in)
    }
    r
  }

  final def fieldAttribute(count : Int, t : FieldType[_]) : HashSet[FieldRestriction[_]] = {
    val r = new HashSet[FieldRestriction[_]]

    var i = count
    while (0 != i) {
      i -= 1

      throw new ParseException(s"Serialized field attributes are currently unsupported.", in)
    }
    r
  }

  /**
   * Parse type definitions and merge them into the known type hierarchy
   */
  final def typeDefinitions {
    var index = 0;
    var THH = 0;
    // the index of the next known class at index THH
    var nextID = new Array[Int](32)
    // the nextName, null if there is no next PD
    var nextName = pb.name(0);

    var p : Pool[_ <: Obj] = null
    var last : Pool[_ <: Obj] = null
    var result : Pool[_ <: Obj] = null

    // Name of all seen class names to prevent duplicate allocation of the same pool.
    val seenNames = new IdentityHashMap[String, AnyRef]
    var TCls = in.v32();

    // file state
    var name : String = null;
    var count = 0;
    var superDef : Pool[_ <: Obj] = null;
    var bpo = 0;

    var moreFile = false
    while ({
      moreFile = TCls > 0
      moreFile | null != nextName
    }) {
      // read next pool from file if required
      if (moreFile) {

        // name
        name = strings.idMap(in.v32)

        // static size
        count = in.v32

        // attr
        val attr = {
          val rc = in.v32
          if (0 == rc) new HashSet[TypeRestriction[_]] else typeAttribute(rc)
        }

        // super
        {
          val superID = in.v32
          if (0 == superID) {
            superDef = null
            bpo = 0
          } else if (superID > fdts.size) {
            throw new ParseException(s"""Type $name refers to an ill-formed super type.
  found: $superID; current number of other types: ${fdts.size}""", in);
          } else {
            superDef = fdts(superID - 1).asInstanceOf[Pool[_ <: Obj]]
            bpo = in.v32
          }
        }
      }

      // allocate pool
      var keepKnown = false
      var keepFile = !moreFile
      do {
        keepKnown = null == nextName

        if (moreFile) {
          // check common case, i.e. the next class is the expected one
          if (!keepKnown) {
            if (superDef == p) {
              if (name == nextName) {
                // the next pool is the expected one
                keepFile = false;

              } else if (Parser.compare(name, nextName) < 0) {
                // we have to advance the file pool
                keepKnown = true;
                keepFile = false;

              } else {
                // we have to advance known pools
                keepFile = true;
              }
            } else {

              // depending on the files super THH, we can decide if we have to process the files type or
              // our type first;
              // invariant: p != superDef ⇒ superDef.THH != THH
              // invariant: ∀p. p.next.THH <= p.THH + 1
              // invariant: ∀p. p.Super = null <=> p.THH = 0
              if (null != superDef && superDef.THH < THH) {
                // we have to advance known pools
                keepFile = true;

              } else {
                // we have to advance the file pool
                keepKnown = true;
                keepFile = false;
              }
            }
          } else {
            // there are no more known pools
            keepFile = false;
          }
        } else if (keepKnown) {
          // we are done
          return ;
        }

        // create the next pool
        if (keepKnown) {
          // an unknown pool has to be created
          if (null != superDef) {
            result = superDef.makeSub(index, name);
            index += 1
          } else {
            last = null;
            result = new SubPool(index, name, classOf[UnknownObject], null);
            index += 1
          }
          result.bpo = bpo;
          fdts += result
          classes += result

          // set next
          if (null != last) {
            last.next = result;
          }
          last = result;
        } else {
          if (null != p) {
            p = p.makeSub(nextID(THH), index);
            nextID(THH) += 1
            index += 1

          } else {
            last = null;
            p = pb.make(nextID(0), index);
            nextID(0) += 1
            index += 1
          }
          // @note this is sane, because it is 0 if p is not part of the type hierarchy of superDef
          p.bpo = bpo;
          SIFA(nsID) = p;
          nsID += 1
          classes += p

          if (!keepFile) {
            result = p;
            fdts += result
          }

          // set next
          if (null != last) {
            last.next = p;
          }
          last = p;

          // move to next pool
          {
            // try to move down to our first child
            THH += 1
            nextID(THH) = 0
            nextName = p.nameSub(0)

            // move up until we find a next pool
            while (null == nextName & THH != 1) {
              p = p.superPool;
              THH -= 1
              nextName = p.nameSub(nextID(THH));
            }
            // check at base type level
            if (null == nextName) {
              p = null;
              THH = 0
              nextName = pb.name(nextID(0));
            }
          }
        }
        // check for duplicate adds
        if (seenNames.containsKey(last.name)) {
          throw new OGSSException("duplicate definition of type " + last.name);
        }
        seenNames.put(last.name, null);
      } while (keepFile);

      result.cachedSize = count
      result.staticDataInstances = count

      // add a null value for each data field to ensure that the temporary size of data fields matches those
      // from file
      var fields = in.v32();
      while (fields != 0) {
        fields -= 1
        result.dataFields += null
      }

      TCls -= 1
    }
  }

  final def TContainer {
    // next type ID
    var tid = 10 + classes.size
    // KCC index
    var ki = 0;
    // @note it is always possible to construct the next kcc from SIFA
    var kcc = pb.kcc(0);
    var kkind = 0;
    var kb1 : FieldType[_] = null
    var kb2 : FieldType[_] = null
    // @note using int here means that UCC may only contain TIDs < 2^14
    var lastUCC = 0;
    var kucc = 0;
    if (-1 != kcc) {
      kkind = (kcc >> 30) & 3;
      kb1 = SIFA(kcc & 0x7FFF)
      kb2 = if (3 == kkind) SIFA((kcc >> 15) & 0x7FFF) else null
      kucc = Parser.toUCC(kkind, kb1, kb2);
    }

    var count = in.v32()
    while (0 != count) {
      count -= 1

      val fkind = in.i8
      val fb1 = fieldType()
      val fb2 = if (3 == fkind) fieldType() else null
      val fucc = Parser.toUCC(fkind, fb1, fb2);

      var r : ContainerType[_] = null
      var cmp = -1;

      // construct known containers until we hit the state of the file
      try {
        while (-1 != kcc && {
          cmp = (fucc - kucc)
          cmp >= 0
        }) {
          r = kkind match {
            case 0 ⇒ new ArrayType(tid, kb1);
            case 1 ⇒ new ListType(tid, kb1);
            case 2 ⇒ new SetType(tid, kb1);
            case 3 ⇒ new MapType(tid, kb1, kb2);
          }
          tid += 1
          SIFA(nsID) = r;
          nsID += 1
          r.fieldID = nextFieldID
          nextFieldID += 1
          containers += r

          // check UCC order
          if (lastUCC > kucc) {
            throw new ParseException("File is not UCC-ordered.", in);
          }
          lastUCC = kucc;

          // move to next kcc
          ki += 1
          kcc = pb.kcc(ki);
          if (-1 != kcc) {
            kkind = (kcc >> 30) & 3;
            kb1 = SIFA(kcc & 0x7FFF)
            kb2 = if (3 == kkind) SIFA((kcc >> 15) & 0x7FFF) else null
            kucc = Parser.toUCC(kkind, kb1, kb2);
          }

          // break loop for perfect matches after the first iteration
          if (0 == cmp)
            throw new Error;
        }
      } catch {
        case e : Error ⇒ //simulated break
      }

      // the last constructed kcc was not the type from the file
      if (0 != cmp) {
        r = fkind match {
          case 0 ⇒ new ArrayType(tid, fb1);
          case 1 ⇒ new ListType(tid, fb1);
          case 2 ⇒ new SetType(tid, fb1);
          case 3 ⇒ new MapType(tid, fb1, fb2);
        }
        tid += 1

        r.fieldID = nextFieldID
        nextFieldID += 1
        containers += r

        // check UCC order
        if (lastUCC > fucc) {
          throw new ParseException("File is not UCC-ordered.", in);
        }
        lastUCC = fucc;
      }
      fields += r
      fdts += r
    }

    // construct remaining known containers
    while (-1 != kcc) {
      val r : ContainerType[_] = kkind match {
        case 0 ⇒ new ArrayType(tid, kb1);
        case 1 ⇒ new ListType(tid, kb1);
        case 2 ⇒ new SetType(tid, kb1);
        case 3 ⇒ new MapType(tid, kb1, kb2);
      }
      tid += 1
      SIFA(nsID) = r;
      nsID += 1
      r.fieldID = nextFieldID
      nextFieldID += 1
      containers += r

      // check UCC order
      if (lastUCC > kucc) {
        throw new ParseException("File is not UCC-ordered.", in);
      }
      lastUCC = kucc;

      // move to next kcc
      ki += 1
      kcc = pb.kcc(ki);
      if (-1 != kcc) {
        kkind = (kcc >> 30) & 3;
        kb1 = SIFA(kcc & 0x7FFF)
        kb2 = if (3 == kkind) SIFA((kcc >> 15) & 0x7FFF) else null
        kucc = Parser.toUCC(kkind, kb1, kb2);
      }
    }
  }

  final def TEnum {
    // next type ID
    var tid = 10 + classes.size + containers.size

    var ki = 0;
    var nextName = pb.enumName(ki);
    var r : EnumPool[_ <: Enumeration] = null
    // create enums from file
    var count = in.v32()
    while (0 != count) {
      count -= 1

      var name = strings.idMap(in.v32);
      var vcount = in.v32
      if (vcount <= 0)
        throw new ParseException(s"Enum $name is too small.", in);

      val vs = new Array[String](vcount)
      for (i ← 0 until vcount) {
        vs(i) = strings.idMap(in.v32);
      }

      var cmp = if (null != nextName) Parser.compare(name, nextName) else -1

      var break = true
      while (break) {
        if (0 == cmp) {
          r = EnumPool(tid, name, vs, pb.enumMake(ki));
          tid += 1
          ki += 1
          enums += r
          fdts += r
          SIFA(nsID) = r;
          nsID += 1
          nextName = pb.enumName(ki);
          break = false

        } else if (cmp < 1) {
          r = EnumPool(tid, name, vs, null);
          tid += 1
          enums += r
          fdts += r
          break = false

        } else {
          r = EnumPool(tid, nextName, null, pb.enumMake(ki));
          tid += 1
          ki += 1
          enums += r
          SIFA(nsID) = r;
          nsID += 1
          nextName = pb.enumName(ki);
          cmp = if (null != nextName) Parser.compare(name, nextName) else -1
        }
      }
    }
    // create remaining known enums
    while (null != nextName) {
      r = EnumPool(tid, nextName, null, pb.enumMake(ki));
      tid += 1
      ki += 1
      enums += r
      SIFA(nsID) = r;
      nsID += 1
      nextName = pb.enumName(ki);
    }
  }

  /**
   * parse T and F
   */
  def typeBlock : Unit

  final def readFields(p : Pool[_ <: Obj]) {
    // we have not yet seen a known field
    var ki = 0;

    // we pass the size by adding null's for each expected field in the stream because AL.clear does not return
    // its backing array, i.e. we will likely not resize it that way
    var idx = p.dataFields.size

    p.dataFields.clear
    var kfn = p.KFN(0);
    while (0 != idx) {
      idx -= 1

      // read field
      val name = strings.idMap(in.v32);
      val t = fieldType
      val rest = {
        val rc = in.v32
        if (0 == rc) new HashSet[FieldRestriction[_]] else fieldAttribute(rc, t)
      }
      var f : Field[_, _] = null

      try {
        while ({
          kfn = p.KFN(ki)
          null != kfn
        }) {
          // is it the next known field?
          if (name == kfn) {
            f = p.KFC(ki, SIFA, nextFieldID)
            ki += 1
            if (f.isInstanceOf[AutoField[_, _]])
              throw new ParseException(s"Found transient field ${p.name}.$name in the file.", in);

            if (f.t != t)
              throw new ParseException(s"Field ${p.name}.${f.name} should have type ${f.t} but has type $t", in);

            // break
            throw new Error;
          }

          // else, it might be an unknown field
          if (Parser.compare(name, kfn) < 0) {
            // create unknown field
            f = new LazyField(t, name, nextFieldID, p);
            // break
            throw new Error;
          }

          // else, it is a known fields not contained in the file
          f = p.KFC(ki, SIFA, nextFieldID);
          ki += 1
          if (!f.isInstanceOf[AutoField[_, _]]) {
            nextFieldID += 1

            // increase maxDeps
            f.t match {
              case t : HullType[_] ⇒ t.maxDeps += 1
              case _               ⇒
            }
          }
          f = null;
        }
      } catch {
        case e : Error ⇒ // simulated break
      }

      if (null == f) {
        // no known fields left, so it is obviously unknown
        f = new LazyField(t, name, nextFieldID, p);

        nextFieldID += 1

        // increase maxDeps
        f.t match {
          case t : HullType[_] ⇒ t.maxDeps += 1
          case _               ⇒
        }
      }

      f.restrictions.asInstanceOf[HashSet[Any]] ++= rest.asInstanceOf[HashSet[Any]]

      fields += f
    }

    // create remaining auto fields
    if (kfn != null)
      do {
        p.KFC(ki, SIFA, nextFieldID)
        nextFieldID += 1
        ki += 1
      } while (null != p.KFN(ki))
  }

  /**
   * Jump through HD-entries to create read tasks
   */
  def processData : Unit

  // G
  {
    val first = in.i8
    // guard is 22 26?
    if (first == 0x22) {
      guard = if (in.i8 == 0x26) "" else null
    } // guard is hash?
    else if (first == 0x23) {
      val buf = new ByteArrayOutputStream
      var next = in.i8
      while (0 != next) {
        buf.write(next);
        next = in.i8
      }
      guard = new String(buf.toByteArray(), StringPool.utf8);

    }
    if (null == guard) {
      throw new ParseException("Illegal guard.", in);
    }
  }

  // S
  try {
    fields += strings
    strings.readSL(in)
  } catch {
    case e : Exception ⇒ throw new ParseException("corrupted string block", in, e);
  }

  // T
  try {
    typeBlock
  } catch {
    case e : Exception ⇒ throw new ParseException("corrupted type block", in, e);
  }

  fixContainerMD

  // HD
  processData

  if (!in.eof()) {
    throw new ParseException("Expected end of file, but some bytes remain.", in);
  }
}

object Parser {
  /**
   * File size in bytes below which the sequential parser will be used.
   */
  var SEQ_LIMIT = 512000

  /**
   * Correct and more efficient string compare.
   */
  def compare(L : String, R : String) : Int = {
    if (L == R)
      return 0;

    val len1 = L.length
    val len2 = R.length
    if (len1 != len2)
      return len1 - len2;

    val v1 = L.toCharArray
    val v2 = R.toCharArray

    var i = 0
    while (i < len1) {
      val c1 = v1(i)
      val c2 = v2(i)
      if (c1 != c2) {
        return c1 - c2
      }
      i += 1
    }
    return 0;
  }

  /**
   * turn kcc into ucc; this is always possible for the next type
   *
   * @return the UCC for a given kcc
   */
  def toUCC(kind : Int, b1 : FieldType[_], b2 : FieldType[_]) : Int = {
    val baseTID1 = b1.typeID;
    val baseTID2 = if (null == b2) 0 else b2.typeID

    if (baseTID2 < baseTID1) {
      (baseTID1 << 17) | (kind << 15) | baseTID2;
    } else {
      (baseTID2 << 17) | (kind << 15) | baseTID1;
    }
  }
}

private abstract class Job extends Runnable {
  override def run;
}
