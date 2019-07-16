package ogss.common.scala.internal

/**
 * iterates efficiently over the type hierarchy
 *
 * @author Timm Felden
 */
final class TypeHierarchyIterator[T <: Obj](
  private var p : Pool[_ <: T]
) extends Iterator[Pool[_ <: T]] {
  val end = p.THH

  override def hasNext = null != p

  override def next = {
    val r = p
    val n = p.next.asInstanceOf[Pool[_ <: T]]

    p = if (null != n && end < n.THH) n else null

    r
  }

  /**
   * @note valid, iff hasNext
   * @return the current element
   */
  def get = p
}