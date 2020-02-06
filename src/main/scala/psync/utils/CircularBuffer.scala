package psync.utils

import scala.reflect.ClassTag

class CircularBuffer[T: ClassTag](protected val capacity: Int, protected val default: T, protected var current: Int = 0) {
  
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
    assert(b == 1, "capacity should be a power of 2.")
    mask
  }

  protected final val mask = pow2(capacity)
  @inline
  protected final def index(i: Int) = i & mask

  protected val values = Array.fill[T](capacity)(default)
  
  def inRange(i: Int): Boolean = {
    val diff = i - current
    diff >= 0 && diff < capacity 
  }
  
  protected def _get(i: Int): T = values(index(i))
  
  def get(i: Int): T = if (inRange(i)) _get(i) else default
  
  def getCurrent: T = _get(current)

  def next: Unit = {
    values(index(current)) = default
    current += 1
  }
  
  def prev: Unit = {
    current -= 1
    values(index(current)) = default
  }
  
  def getSafe(i: Int): Option[T] = {
    if (inRange(i)) Some(values(index(i)))
    else None
  }

  def set(i: Int, value: T): Boolean = {
    if (inRange(i)) {
      values(index(i)) = value
      true
    } else {
      false
    }
  }
  
  def reset: Unit = {
    for (i <- 0 until capacity)
      values(i) = default
    current = 0
  }


}
