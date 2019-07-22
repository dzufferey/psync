package psync.utils

import psync.Time

class LookaheadCounter(maxLookahead: Int, currentTime: Time = new Time(0)) extends CircularBuffer[Int](maxLookahead, 0, currentTime.toInt) {

  def increase(t: Time): Int = {
    if (inRange(t.toInt)) {
      val idx = index(t.toInt)
      val v = values(idx) + 1
      values(idx) = v
      v
    } else {
      0
    }
  }
  
  def decrease(t: Time): Int = {
    if (inRange(t.toInt)) {
      val idx = index(t.toInt)
      val v = values(idx) - 1
      values(idx) = v
      v
    } else {
      0
    }
  }
  
  def get(t: Time): Int = get(t.toInt)

}
