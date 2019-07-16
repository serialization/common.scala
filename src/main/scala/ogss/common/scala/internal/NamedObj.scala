package ogss.common.scala.internal

/**
 * An Obj that holds a pointer to its pool.
 *
 * @author Timm Felden
 *
 * @note This type definition is in internal, because we have to protect the user from tampering with ID
 */
trait NamedObj extends Obj {
  def τp : Pool[_]
}