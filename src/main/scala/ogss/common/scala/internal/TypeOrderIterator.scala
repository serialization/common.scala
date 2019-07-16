package ogss.common.scala.internal

/**
 * Iterates efficiently over dynamic instances of a pool in type order.
 *
 * @author Timm Felden
 * @note cast required to work around weakened type system by javac 1.8.131
 */
final class TypeOrderIterator[T <: Obj](_p : Pool[T]) extends Iterator[T] {
  val ts = new TypeHierarchyIterator(_p)
  var is = {
    while (ts.hasNext && 0 == ts.get.staticSize) {
      ts.next
    }
    if (ts.hasNext)
      new StaticDataIterator(ts.get)
    else
      null
  }

  override def hasNext = null != is

  override def next = {
    val r = is.next
    if (!is.hasNext) {
      is = {
        while (ts.hasNext && 0 == ts.get.staticSize) {
          ts.next
        }
        if (ts.hasNext)
          new StaticDataIterator(ts.get)
        else
          null
      }
    }
    r
  }
}