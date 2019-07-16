package ogss.common.scala.internal

import ogss.common.scala.api.FieldAccess
import ogss.common.scala.internal.fields.AutoField
import scala.collection.mutable.HashSet
import ogss.common.streams.MappedInStream
import ogss.common.streams.BufferedOutStream
import ogss.common.scala.internal.restrictions.FieldRestriction

/**
 * Actual implementation as used by all bindings.
 *
 * @author Timm Felden
 */
abstract class Field[T, Ref <: Obj](
  override val t :     FieldType[T],
  override val name :  String,
  val ID :             Int,
  override val owner : Pool[Ref]
) extends FieldAccess[T] {

  // register field
  if (ID < 0)
    // auto fields get per-type negative IDs
    owner.autoFields(-1 - ID) = this.asInstanceOf[AutoField[T, Ref]]
  else
    owner.dataFields += this

  /**
   * The current number of pending blocks. 0 if FD is not split into blocks. This number is only meaningful while
   * writing a file.
   */
  var blocks = 0

  /**
   * Restriction handling.
   */
  val restrictions = new HashSet[FieldRestriction[T]]

  /**
   * Check consistency of restrictions on this field.
   */
  private[internal] final def check() {
    this match {
      case self : LazyField[T, Ref] ⇒ self.ensureLoaded
      case _                        ⇒
    }

    if (!restrictions.isEmpty)
      for (
        x ← owner if !x.isDeleted;
        r ← restrictions
      ) r.check(get(x));
  }

  /**
   * Read data from a mapped input stream and set it accordingly. This is invoked at the very end of state
   * construction and done massively in parallel.
   */
  def read(i : Int, h : Int, in : MappedInStream)

  /**
   * write data into a map at the end of a write/append operation
   *
   * @note only called, if there actually is field data to be written
   * @return true iff the written data contains default values only
   */
  def write(i : Int, h : Int, out : BufferedOutStream) : Boolean

  override def toString = t.toString + " " + name
}