package ogss.common.scala.internal

/**
 * Follow the subtyping oriented variant of EnumProxies by Sarah Stieß. In contrast to her solution, we will only
 * instantiate this class directly if T is not contained in the tool specification.
 *
 * @author Timm Felden
 */
final class EnumProxy[T <: Enumeration](
  /**
   * Points to the target enum value, if it is part of the tool specification.
   *
   * @note null iff the target is not part of the tool specification.
   */
  val target : T#Value,

  /**
 * The name of this enum value. Names are stable.
 */
  val name : String,

  /**
 * The ID of this enum value. IDs are not stable as they depend on the input file.
 */
  val ID : Int
) {

  /**
   * The pool owning this enum value. It can be used to discover other enum values contained in this file.
   */
  private[internal] var _owner : EnumPool[T] = _
  def owner = _owner
}