package ogss.common.scala.internal

import ogss.common.scala.api.ParseException
import ogss.common.scala.api.OGSSException

/**
 * Singletons instantiate this trait to improve the API.
 *
 * @author Timm Felden
 */
trait SingletonPool[T <: Obj] extends Pool[T] {

  final lazy val theInstance = make
  final def get = theInstance

  final override def allocateInstances {
    var i = bpo

    if (staticDataInstances > 1) {
      throw new OGSSException(s"classe $name is a singleton, but has $staticDataInstances instances")
    }
    if (1 == staticDataInstances) {
      // create a new object, claiming that there is none in data
      staticDataInstances = 0
      val v = get
      // instance is not a new object and make the object an object obtained from file
      this.newObjects.clear()
      staticDataInstances = 1

      data(i) = v
      v._ID = i + 1
    }
  }
}