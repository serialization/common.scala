package ogss.common.scala.internal

/**
 * Iterator over all fields declared by an access, i.e. excluding fields of
 * super types.
 *
 * @author Timm Felden
 */
final class StaticFieldIterator(
  private var p : Pool[_]
) extends Iterator[Field[_, _]] {

  private var i = -p.autoFields.length
  // find first valid state
  if (i == 0 && 0 == p.dataFields.size) {
    this.p = null
  }

  override def hasNext = p != null

  override def next : Field[_, _] = {
    val f : Field[_, _] =
      if (i < 0) p.autoFields(-1 - i)
      else p.dataFields(i)

    i += 1

    if (i == p.dataFields.size) {
      p = null
    }
    return f;
  }
}
