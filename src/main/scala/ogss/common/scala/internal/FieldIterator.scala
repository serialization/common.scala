package ogss.common.scala.internal

/**
 * Iterator over all fields of an access.
 *
 * @author Timm Felden
 */
final class FieldIterator(
  private var p : Pool[_]
) extends Iterator[Field[_, _]] {

  private var i = -p.autoFields.length
  // find first valid state
  while (this.p != null && i == 0 && 0 == p.dataFields.size) {
    this.p = this.p.superPool;
    if (this.p != null)
      this.i = -this.p.autoFields.length;
  }

  override def hasNext = p != null

  override def next : Field[_, _] = {
    val f : Field[_, _] =
      if (i < 0) p.autoFields(-1 - i)
      else p.dataFields(i)

    i += 1

    if (i == p.dataFields.size) {
      do {
        p = p.superPool;
        if (p != null)
          i = -p.autoFields.length;
      } while (p != null && i == 0 && 0 == p.dataFields.size)
    }
    return f;
  }
}
