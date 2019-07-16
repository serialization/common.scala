package ogss.common.scala.internal

/**
 * The root of the class instance hierarchy in OGSS.
 *
 * @author Timm Felden
 * @note This type definition is in internal, because we have to protect the user from tampering with ID
 */
abstract class Obj(id : Int) {

  /**
   * negative for new objects<br>
   * 0 for deleted objects<br>
   * everything else is the ID of an object inside a file
   *
   * @note semantics of negative IDs may be subject to change without further notice
   */
  protected[internal] var _ID : Int = id;

  /**
   * Do not rely on ID if you do not know exactly what you are doing
   */
  final def ID : Int = _ID;

  /**
   * @return whether the object has been deleted
   */
  final def isDeleted = 0 == ID

  /**
   * @return the SIFA type id of the type of this object or -1 if none is available
   * @note this should be a class val
   */
  def STID : Int;

  /**
   * potentially expensive but more pretty representation of this instance.
   */
  final def prettyString(sf : State) : String = {
    val sb = new StringBuilder("(this: ").append(this);
    for (f ← sf.pool(this).allFields) {
      sb.append(", ").append(f.name).append(": ").append(f.get(this));
    }
    return sb.append(")").toString();
  }
}