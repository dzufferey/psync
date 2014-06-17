package round.utils

import scala.collection.mutable.HashMap

class Namer {

  private val map = new java.util.concurrent.ConcurrentHashMap[String, java.util.concurrent.atomic.AtomicInteger]()

  private def counter = new java.util.concurrent.atomic.AtomicInteger()

  def apply(prefix: String, preserve: Boolean = false): String = {
    val realPrefix = if (preserve) prefix else prefix.replaceFirst("\\$\\d*\\z", "")
    val c1 = counter
    val c2 = map.putIfAbsent(prefix, c1)
    val c3 = if (c2 == null) c1 else c2
    realPrefix.trim + "$" + c3.incrementAndGet
  }

}

object Namer extends Namer {
}
