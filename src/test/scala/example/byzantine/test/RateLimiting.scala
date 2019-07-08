package example.byzantine.test

//TODO borrowed from Runner

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

/** Rate limiting (sliding window) */
trait RateLimiting {
  self: Runner =>

  import self._

  var rate = options.rate

  def acquire {
    assert(lck.isHeldByCurrentThread())
    Logger("Runner", Debug, id + ", taking")
    while(rate <= 0) {
      monitor.await
    }
    rate -= 1
  }

  def release {
    assert(lck.isHeldByCurrentThread())
    Logger("Runner", Debug, id + ", releasing")
    rate += 1
    monitor.signal()
  }

}
