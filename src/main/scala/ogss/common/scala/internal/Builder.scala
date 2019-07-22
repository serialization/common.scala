package ogss.common.scala.internal

/**
 * Builder for new instances of the pool.
 *
 * @author Timm Felden
 * @note the generic co-hierarchy is used to compress the builder hierarchy where possible
 */
class Builder[T <: Obj](
  private var p : Pool[T],
  val self :      T
) {

  /**
   * registers the object and invalidates the builder
   *
   * @return the created object
   */
  def make : T = {
    if (null == p) throw new IllegalStateException("builder has been consumed already")

    p.add(self)
    // invalidate to prevent duplicate registration
    p = null
    self
  }
}