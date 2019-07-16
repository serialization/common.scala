package ogss.common.scala.api

import ogss.common.streams.InStream
import ogss.common.scala.internal.Field
import java.nio.BufferUnderflowException

/**
 * Super type for all OGSS-related errors reports
 *
 * @author Timm Felden
 */
class OGSSException(message : String = null, cause : Throwable = null) extends Exception(
  if (null == message & null != cause) cause.getMessage else message,
  cause
);

/**
 * Thrown, if an index into a pool is invalid.
 *
 * @author Timm Felden
 */
class InvalidPoolIndexException(index : Long, size : Int, pool : String, cause : Exception = null)
  extends OGSSException(
    message = s"Invalid index $index into pool $pool of size $size",
    cause = cause
  );

/**
 * This exception is used if byte stream related errors occur.
 *
 * @author Timm Felden
 */
class ParseException(msg : String, in : InStream = null, cause : Throwable = null)
  extends OGSSException(
    message = if (null == in) msg else f"At 0x${in.position}%x: $msg",
    cause = cause
  );

/**
 * Thrown, if field deserialization consumes less bytes then specified by the header.
 *
 * @author Timm Felden
 */
class PoolSizeMissmatchError private (_msg : String, _cause : Throwable)
  extends OGSSException(_msg, _cause) {

  def this(position : Long, begin : Long, end : Long, field : Field[_, _]) {
    this(
      f"Corrupted data chunk at 0x$position%X between 0x$begin%X and 0x$end%X in Field ${field.owner.name}.${field.name} of type: ${field.t}",
      null
    )
  }
  def this(begin : Long, end : Long, field : Field[_, _], e : BufferUnderflowException) {
    this(
      f"Corrupted data chunk between 0x$begin%X and 0x$end%X in Field ${field.owner.name}.${field.name} of type: ${field.t}",
      e
    )
  }
}