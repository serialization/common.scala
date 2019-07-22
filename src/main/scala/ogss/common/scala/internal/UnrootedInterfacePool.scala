package ogss.common.scala.internal

import ogss.common.scala.api.GeneralAccess
import ogss.common.streams.InStream
import ogss.common.streams.OutStream
import scala.reflect.ClassTag

/**
 * Holds interface instances for whom no superclass was defined.
 * Serves as an API realization. Ensures correctness of reflective type system.
 *
 * @author Timm Felden
 */
final class UnrootedInterfacePool[T <: Obj : ClassTag](
  val name :      String,
  val superType : AnyRefType,

  // @note realizations must be in type order
  val realizations : Pool[_ <: T]*
) extends FieldType[T](8) with GeneralAccess[T] {

  override def size : Int = {
    var r = 0;
    for (p ← realizations) {
      r += p.size
    }
    r
  }

  override def iterator : InterfaceIterator[T] = new InterfaceIterator[T](realizations.iterator)

  override def owner = realizations.head._owner

  override def r(in : InStream) : T = superType.r(in).asInstanceOf[T]

  override def w(ref : T, out : OutStream) : Boolean = superType.w(ref, out)

  override def typeCheck(x : Any) : Boolean = x match {
    case x : T ⇒ true
    case _     ⇒ false
  }

  override def get(ID : Int) : T = throw new NoSuchMethodError()
}