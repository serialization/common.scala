package ogss.common.scala.internal.fields

import ogss.common.scala.internal.Field
import ogss.common.scala.internal.FieldType
import ogss.common.scala.internal.Obj
import ogss.common.scala.internal.Pool
import ogss.common.streams.BufferedOutStream
import ogss.common.streams.MappedInStream

/**
 * This trait marks auto fields.
 *
 * @author Timm Felden
 */
abstract class AutoField[T, Ref <: Obj](
  _t :     FieldType[T],
  _name :  String,
  _index : Int,
  _owner : Pool[Ref]
) extends Field[T, Ref](_t, _name, _index, _owner)
  with KnownField {

  final override def read(i : Int, h : Int, in : MappedInStream) =
    throw new NoSuchMethodError("one cannot read auto fields!")

  final override def write(i : Int, h : Int, out : BufferedOutStream) =
    throw new NoSuchMethodError("one cannot write auto fields!")
}