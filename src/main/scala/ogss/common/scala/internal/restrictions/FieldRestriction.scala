package ogss.common.scala.internal.restrictions

/**
 * A restriction that can be applied to a field.
 *
 * TODO should be renamed to attribute
 *
 * @author Timm Felden
 * @param <T>
 *            The Scala type of the field.
 */
abstract class FieldRestriction[T] {

  /**
   * Checks a value and throws an exception in case of error. We prefer the
   * exception throwing mechanism over return values, because we expect checks
   * to fail almost never.
   *
   * @param value
   *            the value to be checked
   */
  def check(value : T) : Unit
}