package ogss.common.scala.internal

/**
 * Iterates efficiently over static instances of a pool.
 *
 * @author Timm Felden
 */
final class StaticDataIterator[T <: Obj](val p : Pool[T]) extends Iterator[T] {
  var index = p.bpo
  var last = index + p.staticDataInstances

  var second = false

  // find first valid position
  if (index == last) {
    second = true
    index = 0
    last = p.newObjects.size
  }

  override def hasNext = index < last

  override def next : T =
    if (!second) {
      val r = p.data(index)
      index += 1
      if (index == last) {
        second = true;
        index = 0;
        last = p.newObjects.size
      }
      r
    } else {

      val r = p.newObjects(index);
      index += 1
      r
    }
}