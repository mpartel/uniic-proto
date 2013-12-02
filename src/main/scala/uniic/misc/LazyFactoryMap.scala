package uniic.misc
import scala.collection.mutable

/** If invoked with a key for which a value does not exist, creates and stores a new value. */
class LazyFactoryMap[K, T](factory: K => T) extends (K => T) {
  private val map = mutable.Map.empty[K, T]

  def apply(key: K): T = {
    map.getOrElseUpdate(key, factory(key))
  }

  def values = map.values
}
