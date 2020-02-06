package example.batching

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

/** Rate limiting (sliding window) */
trait RateLimiting {
  self: BatchingClient =>

  import self._

  var rate = options.rate

  def acquire: Unit = {
    assert(lck.isHeldByCurrentThread())
    Logger("BatchingClient", Debug, s"$id, taking")
    while(rate <= 0) {
      monitor.await
    }
    rate -= 1
  }

  def release: Unit = {
    assert(lck.isHeldByCurrentThread())
    Logger("BatchingClient", Debug, s"$id, releasing")
    rate += 1
    monitor.signal()
  }

}
