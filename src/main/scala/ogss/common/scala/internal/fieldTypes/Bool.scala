package ogss.common.scala.internal.fieldTypes

import ogss.common.streams.MappedInStream
import ogss.common.streams.InStream
import ogss.common.scala.internal.FieldType
import ogss.common.streams.OutStream

/**
 * @author Timm Felden
 */
case object Bool extends FieldType[Boolean](0) {

  override def r(in : InStream) = in.asInstanceOf[MappedInStream].bool

  override def w(v : Boolean, out : OutStream) =
    throw new NoSuchMethodError("the caller has to wrap out!")

  override def toString = "bool"
}