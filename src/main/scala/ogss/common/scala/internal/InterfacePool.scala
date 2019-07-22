package ogss.common.scala.internal

import scala.reflect.ClassTag

import ogss.common.scala.api.GeneralAccess
import ogss.common.streams.InStream
import ogss.common.streams.OutStream

/**
 * Holds interface instances. Serves as an API realization. Ensures correctness of reflective type system.
 *
 * @param T
 *            the type of the interface
 * @param B
 *            the super class of the interface
 *
 * @author Timm Felden
 */
final class InterfacePool[T <: B : ClassTag, B <: Obj](
  override val name : String,
  val superPool :     Pool[B],
  // @note realizations must be in type order
  val realizations : Pool[_ <: T]*
) extends FieldType[T](superPool.typeID) with GeneralAccess[T] {

  override def size : Int = {
    var r = 0;
    for (p ← realizations) {
      r += p.size
    }
    r
  }

  override def iterator : InterfaceIterator[T] = new InterfaceIterator[T](realizations.iterator)

  override def owner = superPool._owner

  override def r(in : InStream) : T = {
    val index = in.v32() - 1;
    val data = superPool.data

    if (index < 0 | data.length <= index) null.asInstanceOf[T]
    else data(index).asInstanceOf[T]
  }

  override def w(ref : T, out : OutStream) : Boolean = {
    if (null == ref) {
      out.i8(0.toByte);
      true
    } else {
      out.v64(ref.ID);
      false
    }
  }

  override def typeCheck(x : Any) : Boolean = x match {
    case x : T ⇒ true
    case _     ⇒ false
  }

  private[internal] final def check {
    // check that realizations yield only instances that are actually an instance of T
    for (p ← realizations; i ← p) {
      if (!i.isInstanceOf[T]) {
        superPool.owner.checkErrors.add(s"$i does not implement interface $name")
      }
    }
  }

  override def get(ID : Int) : T = superPool.get(ID).asInstanceOf[T]
}