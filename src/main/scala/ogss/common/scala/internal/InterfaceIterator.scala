package ogss.common.scala.internal

/**
 * Returns all instances for an interface pool.
 *
 * @author Timm Felden
 */
final class InterfaceIterator[T <: Obj](private val ps : Iterator[Pool[_ <: T]]) extends Iterator[T] {

  private var xs : DynamicDataIterator[_ <: T] = _
  do {
    xs = ps.next.iterator
  } while (ps.hasNext && !xs.hasNext)

  override def hasNext = xs.hasNext

  override def next : T = {
    val r = xs.next

    while (!xs.hasNext && ps.hasNext) {
      xs = ps.next.iterator
    }
    r
  }
}