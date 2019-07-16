package ogss.common.scala.internal

/**
 * Represents an instance of an unknown type hierarchy.
 *
 * @author Timm Felden
 */
final class UnknownObject(
  override val τp : Pool[_],
  _ID :             Int
) extends Obj(_ID) with NamedObj {

  override def STID = -1
}