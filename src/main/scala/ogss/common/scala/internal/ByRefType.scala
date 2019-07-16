package ogss.common.scala.internal

/**
 * This type subsumes all types whose instances have by-ref semantics. Those are Strings, Containers and Objects.
 * All by-ref types assign IDs to their instances and are, hence, iterable.
 *
 * @note ByRefTypes have per-state unique names
 * @author Timm Felden
 */
abstract class ByRefType[T <: AnyRef](_typeID : Int) extends FieldType[T](_typeID);