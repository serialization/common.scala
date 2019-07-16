package ogss.common.scala.internal

import ogss.common.scala.internal.fieldTypes.ArrayType
import ogss.common.scala.internal.fieldTypes.ListType
import ogss.common.scala.internal.fieldTypes.SetType
import ogss.common.scala.internal.fieldTypes.MapType
import ogss.common.scala.internal.fields.AutoField
import ogss.common.scala.internal.fieldTypes.ContainerType

/**
 * Create an empty state. The approach here is different from the generated initialization code in OGSS to reduce the
 * amount of generated code.
 *
 * @author Timm Felden
 */
final class Creator(pb : PoolBuilder) extends StateInitializer(pb) {
  guard = "";

  try {
    // Create Classes
    {
      var index = 0
      var THH = 0;
      // the index of the next known class at index THH
      // @note to self: in C++ this should be string*[32]
      val nextID = new Array[Int](32)
      var nextName = pb.name(0);

      var p : Pool[_ <: Obj] = null
      var last : Pool[_ <: Obj] = null

      while (null != nextName) {
        if (0 == THH) {
          last = null;
          p = pb.make(nextID(0), index)
          nextID(0) += 1
          index += 1
        } else {
          p = p.makeSub(nextID(THH), index)
          nextID(THH) += 1
          index += 1
        }

        SIFA(nsID) = p
        nsID += 1
        classes += p

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
          nextName = p.nameSub(0);

          // move up until we find a next pool
          while (null == nextName && THH != 1) {
            p = p.superPool;
            THH -= 1
            nextName = p.nameSub(nextID(THH));
          }
          // check at base type level
          if (null == nextName) {
            THH -= 1
            nextName = pb.name(nextID(0));
          }
        }
      }
    }

    // Execute known container constructors
    var tid = 10 + classes.size;
    {
      var i = 0
      var kcc = pb.kcc(i)
      while (-1 != kcc) {
        val r : ContainerType[_] = ((kcc >> 30) & 3) match {
          case 0 ⇒ new ArrayType(tid, SIFA(kcc & 0x7FFF));
          case 1 ⇒ new ListType(tid, SIFA(kcc & 0x7FFF));
          case 2 ⇒ new SetType(tid, SIFA(kcc & 0x7FFF));
          case 3 ⇒ new MapType(tid, SIFA(kcc & 0x7FFF), SIFA((kcc >> 15) & 0x7FFF));
        }
        tid += 1

        SIFA(nsID) = r;
        nsID += 1
        r.fieldID = nextFieldID
        nextFieldID += 1
        containers += r

        i += 1
        kcc = pb.kcc(i)
      }
    }

    // Construct known enums
    {
      var ki = 0;
      var r : EnumPool[_ <: Enumeration] = null
      var nextName = pb.enumName(ki);
      // create remaining known enums
      while (null != nextName) {
        r = EnumPool(tid, nextName, null, pb.enumMake(ki))
        tid += 1
        ki += 1
        enums += r
        SIFA(nsID) = r;
        nsID += 1
        nextName = pb.enumName(ki);
      }
    }

    // Create Fields
    for (p ← classes) {
      var i = 0
      while (null != p.KFN(i)) {
        p.KFC(i, SIFA, nextFieldID) match {
          case fd : AutoField[_, _] ⇒ // done
          case fd ⇒ {
            nextFieldID += 1

            // increase maxDeps
            fd.t match {
              case t : HullType[_] ⇒ t.maxDeps += 1
              case _               ⇒
            }
          }
        }
        i += 1
      }
    }
  } catch {
    case e : Exception ⇒ throw new Error("failed to create state", e);
  }

  fixContainerMD
}