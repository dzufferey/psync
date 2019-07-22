package psync.utils

/** A Bitset with up to index up to 63.
 *  Any index above that is takend modulo 64! */
class LongBitSet(val store: Long) extends AnyVal {

  def get(pos: Int): Boolean = ((store >>> (pos & 63)) & 1) != 0l
  
  def flip(pos: Int): LongBitSet = new LongBitSet((1l << (pos & 63)) ^ store)

  def set(pos: Int): LongBitSet = new LongBitSet((1l << (pos & 63)) | store)

  def clear(pos: Int): LongBitSet = new LongBitSet(~(1l << (pos & 63)) & store)

  def size: Int = {
    var n = 0
    var loop = store
    while (loop != 0) {
      n += (loop & 1).toInt
      loop = loop >>> 1
    }
    n
  }

}

object LongBitSet {

  def empty = new LongBitSet(0)

  def full = new LongBitSet(-1)

}
