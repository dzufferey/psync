package psync.utils

import psync.Time

class LookaheadCounter(protected val maxLookahead: Int, protected var current: Time = new Time(0)) {

  protected final def pow2(_n: Int): Int = {
    var n = _n
    assert(n > 0)
    var b = 0
    var mask = 0
    while(n != 0) {
      b += (n & 1)
      n = n >>> 1
      mask = (mask << 1) | 1
    }
    mask = mask >>> 1 // 2¹ == 0 so ignore the first bit
    assert(b == 1, "maxLookahead should be a power of 2.")
    mask
  }

  protected final val mask = pow2(maxLookahead)
  @inline
  protected final def index(t: Time) = t.toInt & mask

  protected val counters = Array.ofDim[Int](maxLookahead)

  protected def inRange(t: Time): Boolean = {
    t >= current && t - current < maxLookahead
  }

  def increase(t: Time): Int = {
    if (inRange(t)) {
      val idx = index(t)
      val v = counters(idx) + 1
      counters(idx) = v
      v
    } else {
      -1
    }
  }
  
  def decrease(t: Time): Int = {
    if (inRange(t)) {
      val idx = index(t)
      val v = counters(idx) - 1
      counters(idx) = v
      v
    } else {
      -1
    }
  }
  
  def get(t: Time): Int = {
    if (inRange(t)) counters(index(t))
    else -1
  }
  
  def getCurrent: Int = get(current)

  def next {
    counters(index(current)) = 0
    current = new Time(current.toInt + 1)
  }

  def reset {
    for (i <- 0 until maxLookahead)
      counters(i) = 0
    current = new Time(0)
  }

}
