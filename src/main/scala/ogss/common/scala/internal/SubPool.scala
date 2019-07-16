package ogss.common.scala.internal

/**
 * A generic sub pool class that creates new objects via reflection to reduce the amount of generated code.
 *
 * @author Timm Felden
 */
final class SubPool[T <: Obj](
  _poolIndex :      Int,
  _name :           String,
  private val cls : Class[T],
  _super :          Pool[_ >: T <: Obj]
) extends Pool[T](_poolIndex, _name, _super, 0) {

  protected[internal] override def makeSub(index : Int, name : String) = new SubPool(index, name, cls, this)

  protected[internal] override def allocateInstances {
    var i = bpo
    var j = 0
    val high = i + staticDataInstances

    val make = cls.getConstructor(classOf[Pool[_]], classOf[Int]);
    while (i < high) {
      j = (i + 1)
      data(i) = make.newInstance(this, Int.box(j))
      i = j;
    }
  }

  override def make() = {
    val r = cls.getConstructor(classOf[Pool[_]], classOf[Int]).newInstance(this, Int.box(-1 - newObjects.size))
    newObjects += r
    r
  }
}