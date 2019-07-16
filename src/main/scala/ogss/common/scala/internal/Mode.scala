package ogss.common.scala.internal

import java.io.IOException

import ogss.common.scala.api.Create
import ogss.common.scala.api.Mode
import ogss.common.scala.api.Read
import ogss.common.scala.api.ReadMode
import ogss.common.scala.api.Write
import ogss.common.scala.api.WriteMode

/**
 * Transform a list of modes into create/write flags.
 *
 * @author Timm Felden
 */
final class ActualMode(modes : Seq[Mode]) {
  // determine open mode
  // @note read is preferred over create, because empty files are
  // legal and the file has been created by now if it did not exist
  // yet
  // @note write is preferred over append, because usage is more
  // inuitive
  val (create : Boolean, write : Boolean) = {
    var openMode : ReadMode = null
    var closeMode : WriteMode = null;
    for (m ← modes)
      m match {
        case m : ReadMode ⇒
          if (null == openMode)
            openMode = m;
          else if (openMode != m)
            throw new IOException("You can either create or read a file.");

        case m : WriteMode ⇒
          if (null == closeMode)
            closeMode = m;
          else if (closeMode != m)
            throw new IOException("You can use either write or readOnly.");
      }
    if (null == openMode)
      openMode = Read;
    if (null == closeMode)
      closeMode = Write;

    (openMode == Create,
      closeMode == Write)
  }
}