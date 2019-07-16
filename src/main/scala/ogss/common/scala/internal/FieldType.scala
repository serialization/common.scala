package ogss.common.scala.internal

import ogss.common.streams.OutStream
import ogss.common.streams.InStream

/**
 * Top level implementation of a field type, the runtime representation of a fields type.
 *
 * @param <T> type of field values
 *
 * @note representation of the type system relies on invariants and heavy abuse of type erasure
 *
 * @author Timm Felden
 */
abstract class FieldType[T](_typeID : Int) extends ogss.common.scala.api.FieldType[T](_typeID) {

  /**
   * Read one T out of the stream.
   *
   * @note this function has to be implemented by FieldTypes because of limits of the Java type system (and any other
   *       sane type system)
   * @note intended for internal usage only!
   */
  def r(in : InStream) : T

  /**
   * Write one T into the stream.
   *
   * @note this function has to be implemented by FieldTypes because of limits of the Java type system (and any other
   *       sane type system)
   * @note intended for internal usage only!
   * @return true iff a default value was written
   */
  def w(data : T, out : OutStream) : Boolean
}